package com.avsystem.commons
package redis.actor

import akka.actor.{Actor, ActorRef, Props, Terminated}
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.actor.RedisConnectionActor.PacksResult
import com.avsystem.commons.redis.config.ConnectionConfig
import com.avsystem.commons.redis.exception.{ClientStoppedException, ConnectionClosedException}
import com.avsystem.commons.redis.util.ActorLazyLogging
import com.avsystem.commons.redis.{NodeAddress, RawCommandPacks}

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Actor that manages access to underlying raw connection actor and restarts it when needed. Restarting is done
  * manually (not by standard supervision mechanisms) because this actor needs to update additional state when
  * restarting the connection actor
  */
final class ManagedRedisConnectionActor(address: NodeAddress, config: ConnectionConfig)
  extends Actor with ActorLazyLogging {

  import ManagedRedisConnectionActor._

  def newConnection = context.watch(context.actorOf(Props(new RedisConnectionActor(address, config))))

  private var connectionActor = newConnection
  private val queue = new mutable.Queue[Queued]
  private var reservedBy: Opt[ActorRef] = Opt.Empty
  private var invalid = false

  @tailrec
  def handle(client: ActorRef, packs: RawCommandPacks, reserve: Boolean): Unit =
    if (reservedBy.forall(_ == client)) {
      if (invalid) {
        // connection was restarted and current reservation is invalid
        client ! PacksResult.Failure(new ConnectionClosedException(address))
      } else {
        connectionActor.tell(packs, client)
        if (reserve) {
          log.debug(s"Reserving connection for $client")
          context.watch(client)
          reservedBy = Opt(client)
        } else if (queue.nonEmpty) {
          val q = queue.dequeue()
          handle(q.client, q.packs, q.reserve)
        }
      }
    } else {
      queue += Queued(client, packs, reserve)
    }

  def release(): Unit = {
    log.debug(s"Releasing connection for $reservedBy")
    // resetting state only when invalid == false, because invalid == true means that the connection was restarted
    if (!invalid) {
      connectionActor ! RedisConnectionActor.ResetState
    }
    invalid = false
    reservedBy.foreach(context.unwatch)
    reservedBy = Opt.Empty
    if (queue.nonEmpty) {
      val q = queue.dequeue()
      handle(q.client, q.packs, q.reserve)
    }
  }

  def receive = {
    case packs: RawCommandPacks =>
      handle(sender(), packs, reserve = false)
    case Reserving(packs) =>
      handle(sender(), packs, reserve = true)
    case Release =>
      if (reservedBy.contains(sender())) {
        release()
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

  override def postStop() = if (queue.nonEmpty) {
    val failure = PacksResult.Failure(new ClientStoppedException(address))
    queue.foreach(_.client ! failure)
  }
}

object ManagedRedisConnectionActor {
  case class Reserving(packs: RawCommandPacks)
  case object Release

  case class Queued(client: ActorRef, packs: RawCommandPacks, reserve: Boolean)
}
