package com.avsystem.commons
package redis.actor

import akka.actor.{Actor, ActorRef, Props, Terminated}
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.actor.RedisConnectionActor._
import com.avsystem.commons.redis.config.ManagedConnectionConfig
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
final class ManagedRedisConnectionActor(address: NodeAddress, config: ManagedConnectionConfig)
  extends Actor with ActorLazyLogging {

  import ManagedRedisConnectionActor._

  def newConnection = context.watch(context.actorOf(Props(
    new RedisConnectionActor(address, config.connectionConfig, self))))

  private var connectionActor = newConnection
  // connected and not busy
  private var connectionReady = false
  // connected
  private var connected = false
  private val reserveQueue = new mutable.Queue[QueuedPacks]
  private val writeQueue = new mutable.Queue[Queued]
  private var reservedBy: Opt[ActorRef] = Opt.Empty
  private var reconnectionRetry = 0
  private var reservationInvalid = false

  def handleIncoming(client: ActorRef, packs: RawCommandPacks, reserve: Boolean): Unit =
    if (reservedBy.forall(_ == client)) {
      if (reservationInvalid) {
        // connection was restarted and current reservation is invalid
        client ! PacksResult.Failure(new ConnectionClosedException(address))
      } else {
        if (reserve) {
          log.debug(s"Reserving connection for $client")
          context.watch(client)
          reservedBy = Opt(client)
        }
        handleAllowed(client, packs, reserve)
      }
    } else {
      reserveQueue += QueuedPacks(client, packs, reserve)
    }

  def handleAllowed(client: ActorRef, packs: RawCommandPacks, reserve: Boolean): Unit =
    if (connectionReady) {
      connectionReady = false
      connectionActor.tell(packs, client)
    } else {
      writeQueue += QueuedPacks(client, packs, reserve)
    }

  def handleResetState(): Unit =
    if (connectionReady) {
      connectionReady = false
      connectionActor ! RedisConnectionActor.ResetState
    } else {
      writeQueue += QueuedResetState
    }

  def handleNextAllowed(): Unit =
    if (writeQueue.nonEmpty) {
      writeQueue.dequeue() match {
        case QueuedPacks(client, packs, reserve) => handleAllowed(client, packs, reserve)
        case QueuedResetState => handleResetState()
      }
    }

  def release(): Unit = {
    log.debug(s"Releasing connection for $reservedBy")
    // resetting state only when invalid == false, because invalid == true means that the connection was restarted
    if (!reservationInvalid) {
      handleResetState()
    }
    reservationInvalid = false
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
      connected = true
      connectionReady = true
      handleNextAllowed()
    case InitializationFailure(cause) =>
      failUnfinished(new ConnectionInitializationFailure(cause))
      context.stop(self)
    case Accepted =>
      connectionReady = true
      handleNextAllowed()
    case Terminated(conn) if conn == connectionActor =>
      log.info(s"Connection to Redis on $address was closed.")
      connected = false
      config.reconnectionStrategy.reconnectionDelay(reconnectionRetry) match {
        case Opt(delay) =>
          log.info(s"Reconnecting in $delay (retry $reconnectionRetry) ...")
          reconnectionRetry += 1
          context.system.scheduler.scheduleOnce(delay, context.self, Reconnect)(context.dispatcher)
          if (reservedBy.nonEmpty) {
            reservationInvalid = true
          }
          connectionReady = false
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
        case QueuedResetState =>
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
  case object QueuedResetState extends Queued

  private case object Reconnect
}
