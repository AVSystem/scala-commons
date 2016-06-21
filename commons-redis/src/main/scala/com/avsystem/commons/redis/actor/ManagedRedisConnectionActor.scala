package com.avsystem.commons
package redis.actor

import akka.actor.{Actor, ActorRef, Props, Terminated}
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.exception.{ClientStoppedException, ConnectionClosedException, ConnectionReservedException}
import com.avsystem.commons.redis.util.ActorLazyLogging
import com.avsystem.commons.redis.{NodeAddress, RedisBatch}

import scala.collection.mutable

/**
  * Actor that manages access to underlying raw connection actor and restarts it when needed. Restarting is done
  * manually (not by standard supervision mechanisms) because this actor needs to update additional state when
  * restarting the connection actor
  */
final class ManagedRedisConnectionActor(address: NodeAddress) extends Actor with ActorLazyLogging {

  import ManagedRedisConnectionActor._

  def newConnection = context.watch(context.actorOf(Props(new RedisConnectionActor(address))))

  private var connectionActor = newConnection
  private val reserveQueue = new mutable.Queue[ActorRef]
  private var reservedBy: Opt[ActorRef] = Opt.Empty
  private var invalid = false

  def reserveFor(client: ActorRef): Unit = {
    log.debug(s"Reserving connection for $client")
    invalid = false
    reservedBy.foreach(context.unwatch)
    reservedBy = Opt(client)
    client ! ReserveSuccess
    context.watch(client)
  }

  def release(): Unit = {
    log.debug(s"Releasing connection for $reservedBy")
    // resetting state only when invalid == false, because invalid == true means that the connection was restarted
    if (!invalid) {
      connectionActor ! RedisConnectionActor.ResetState
    }
    if (reserveQueue.nonEmpty) {
      reserveFor(reserveQueue.dequeue())
    } else {
      invalid = false
      reservedBy.foreach(context.unwatch)
      reservedBy = Opt.Empty
    }
  }

  def receive = {
    case Reserve =>
      val s = sender()
      if (reservedBy.forall(_ == s)) {
        reserveFor(s)
      } else {
        reserveQueue += s
      }
    case Release =>
      if (reservedBy.contains(sender())) {
        release()
      }
    case batch: RedisBatch[Any, Nothing] =>
      if (reservedBy.forall(_ == sender())) {
        if (invalid) {
          // connection was restarted and current reservation is invalid
          sender() ! RedisConnectionActor.BatchFailure(new ConnectionClosedException(address))
        } else {
          connectionActor.forward(batch)
        }
      } else {
        sender() ! RedisConnectionActor.BatchFailure(new ConnectionReservedException)
      }
    case Terminated(conn) if conn == connectionActor =>
      log.debug(s"Connection was closed. Reconnecting.")
      connectionActor = newConnection
      if (reservedBy.nonEmpty) {
        invalid = true
      }
    case Terminated(client) if reservedBy.contains(client) =>
      release()
  }

  override def postStop() = if (reserveQueue.nonEmpty) {
    val failure = ReserveFailure(new ClientStoppedException(address))
    reserveQueue.foreach(_ ! failure)
  }
}

object ManagedRedisConnectionActor {
  case object Reserve
  case object Release

  case object ReserveSuccess
  case class ReserveFailure(cause: Throwable)
}
