package com.avsystem.commons
package redis.actor

import akka.actor.{Actor, ActorRef, Props, Terminated}
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.actor.RedisConnectionActor._
import com.avsystem.commons.redis.config.{ConnectionConfig, RetryStrategy}
import com.avsystem.commons.redis.exception._
import com.avsystem.commons.redis.util.ActorLazyLogging
import com.avsystem.commons.redis.{NodeAddress, RawCommandPacks}

import scala.collection.mutable
import scala.concurrent.duration.{Duration, FiniteDuration}

/**
  * Actor that manages access to underlying raw connection actor and restarts it when needed. Restarting is done
  * manually (not by standard supervision mechanisms) because this actor needs to update additional state when
  * restarting the connection actor
  */
final class ManagedRedisConnectionActor(address: NodeAddress, config: ConnectionConfig, reconnectionStrategy: RetryStrategy)
  extends Actor with ActorLazyLogging {

  import ManagedRedisConnectionActor._

  def newConnection = context.actorOf(Props(new RedisConnectionActor(address, config, self)))

  private var connectionActor = newConnection
  // connected and not busy
  private var connectionReady = false
  // contains requests that wait for current reservation to finish
  private val reserveQueue = new mutable.Queue[QueuedPacks]
  // contains requests already allowed to be sent but waiting for the connection client to be available
  private val writeQueue = new mutable.Queue[Queued]
  // client currently holding reservation on this connection (most likely RedisOperationActor)
  private var reservedBy: Opt[ActorRef] = Opt.Empty
  // true when we're in the process of sending some reservation requests to the connection
  private var reservationWriting = false
  // true when reservation currently being written is broken due connection failure
  private var reservationBroken = false
  private var reconnectionRetry = 0

  def handleIncoming(client: ActorRef, packs: RawCommandPacks, reserve: Boolean): Unit =
    if (reservedBy.forall(_ == client)) {
      if (reserve) {
        log.debug(s"Reserving connection for $client")
        context.watch(client)
        reservedBy = client.opt
      }
      handleAllowed(client, packs, reserve)
    } else {
      reserveQueue += QueuedPacks(client, packs, reserve)
    }

  def handleAllowed(client: ActorRef, packs: RawCommandPacks, reserve: Boolean): Unit =
    if (reservationBroken) {
      // connection was restarted and current reservation is broken
      client ! PacksResult.Failure(new ConnectionClosedException(address))
      handleNextAllowed()
    } else if (connectionReady) {
      if (reserve) {
        reservationWriting = true
      }
      connectionReady = false
      connectionActor.tell(packs, client)
    } else {
      writeQueue += QueuedPacks(client, packs, reserve)
    }

  def handleRelease(): Unit =
    if (reservationBroken) {
      // when reservation is broken, don't reset state because connection was restarted anyway and its state was lost
      reservationBroken = false
      reservationWriting = false
      handleNextAllowed()
    } else if (connectionReady) {
      reservationWriting = false
      connectionReady = false
      connectionActor ! RedisConnectionActor.ResetState
    } else {
      writeQueue += QueuedRelease
    }

  def handleNextAllowed(): Unit =
    if (writeQueue.nonEmpty) {
      writeQueue.dequeue() match {
        case QueuedPacks(client, packs, reserve) => handleAllowed(client, packs, reserve)
        case QueuedRelease => handleRelease()
      }
    }

  def release(): Unit = {
    log.debug(s"Releasing connection for $reservedBy")
    handleRelease()
    reservedBy.foreach(context.unwatch)
    reservedBy = Opt.Empty
    while (reservedBy.isEmpty && reserveQueue.nonEmpty) {
      val q = reserveQueue.dequeue()
      handleIncoming(q.client, q.packs, q.reserve)
    }
  }

  def receive = {
    case packs: RawCommandPacks =>
      handleIncoming(sender(), packs, reserve = false)
    case Reserving(packs) =>
      handleIncoming(sender(), packs, reserve = true)
    case Release =>
      if (reservedBy.contains(sender())) {
        release()
      }
    case Initialized =>
      reconnectionRetry = 0
      connectionReady = true
      handleNextAllowed()
    case InitializationFailure(cause) =>
      failUnfinished(new ConnectionInitializationFailure(cause))
      context.stop(self)
    case Accepted =>
      connectionReady = true
      handleNextAllowed()
    case Closed =>
      log.info(s"Connection to Redis on $address was closed.")
      connectionActor ! Stop
      reconnectionStrategy.retryDelay(reconnectionRetry) match {
        case Opt(delay) =>
          log.info(s"Reconnecting in $delay (retry $reconnectionRetry) ...")
          reconnectionRetry += 1
          context.system.scheduler.scheduleOnce(delay, context.self, Reconnect)(context.dispatcher)
          connectionReady = false
          if (reservationWriting) {
            reservationBroken = true
            handleNextAllowed() // in order to fail any remaining requests from this reservation
          }
        case Opt.Empty =>
          log.error(s"All reconnection attempts to $address failed. Stopping.")
          failUnfinished(new ConnectionClosedException(address))
          context.stop(self)
      }
    case Reconnect =>
      connectionActor = newConnection
    case NodeRemoved =>
      connectionActor ! NodeRemoved
      failUnfinished(new NodeRemovedException(address, alreadySent = false))
      context.stop(self)
    case Terminated(client) if reservedBy.contains(client) =>
      release()
  }

  private def failUnfinished(cause: Throwable): Unit = {
    val failure = PacksResult.Failure(cause)
    while (writeQueue.nonEmpty) {
      writeQueue.dequeue() match {
        case QueuedPacks(client, _, _) => client ! failure
        case QueuedRelease =>
      }
    }
    while (reserveQueue.nonEmpty) {
      reserveQueue.dequeue().client ! failure
    }
  }

  override def postStop() =
    failUnfinished(new ClientStoppedException(address))
}

object ManagedRedisConnectionActor {
  case class Reserving(packs: RawCommandPacks)
  case object NodeRemoved
  case object Release

  sealed trait Queued
  case class QueuedPacks(client: ActorRef, packs: RawCommandPacks, reserve: Boolean) extends Queued
  case object QueuedRelease extends Queued

  private case object Reconnect
}
