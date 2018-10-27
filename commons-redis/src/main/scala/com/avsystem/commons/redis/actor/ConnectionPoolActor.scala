package com.avsystem.commons
package redis.actor

import java.util.concurrent.ConcurrentLinkedDeque

import akka.actor.{Actor, ActorRef, Props}
import com.avsystem.commons.redis.NodeAddress
import com.avsystem.commons.redis.actor.ConnectionPoolActor._
import com.avsystem.commons.redis.config.{ConnectionConfig, NodeConfig}
import com.avsystem.commons.redis.exception.RedisException
import com.typesafe.scalalogging.LazyLogging

import scala.annotation.tailrec

class ConnectionPoolActor(address: NodeAddress, config: NodeConfig, queue: ConcurrentLinkedDeque[QueuedConn])
  extends Actor with LazyLogging {

  import context._

  private val connections = new MHashSet[ActorRef]

  if (config.maxBlockingIdleTime.isFinite) {
    val interval = config.blockingCleanupInterval
    system.scheduler.schedule(interval, interval, self, Cleanup)
  }

  def receive: Receive = {
    case CreateNewConnection if connections.size < config.maxBlockingPoolSize =>
      logger.info(s"Creating new blocking connection to $address")
      val connConfig: ConnectionConfig = config.blockingConnectionConfigs(connections.size)
      val props = Props(new RedisConnectionActor(address, connConfig))
      val connection = connConfig.actorName.fold(actorOf(props))(actorOf(props, _))
      connections += connection
      connection ! RedisConnectionActor.Open(mustInitiallyConnect = false, Promise[Unit]())
      sender() ! NewConnection(connection)
    case CreateNewConnection =>
      sender() ! Full
    case Cleanup =>
      cleanup(System.nanoTime(), config.maxBlockingIdleTime.toNanos)
    case Close(cause) =>
      connections.foreach(_ ! RedisConnectionActor.Close(cause))
  }

  private def cleanup(nowNanos: Long, maxIdleNanos: Long): Unit = {
    @tailrec def loop(dequeue: Boolean): Unit = {
      val last = if (dequeue) queue.pollLast() else queue.peekLast()
      last match {
        case QueuedConn(conn, enqueuedAt) =>
          val stale = (nowNanos - enqueuedAt) > maxIdleNanos
          if (!dequeue && stale) {
            loop(dequeue = true)
          } else if (dequeue && stale) {
            conn ! RedisConnectionActor.Close(new RedisException("Idle blocking connection closed"))
            context.stop(conn)
            connections.remove(conn)
            loop(dequeue = false)
          } else if (dequeue && !stale) {
            // unlikely situation where we dequeued something else than we peeked before
            queue.addLast(last)
          }
        case null =>
      }
    }
    loop(dequeue = false)
  }
}
object ConnectionPoolActor {
  case class QueuedConn(conn: ActorRef, enqueuedAt: Long)

  object CreateNewConnection
  case class Close(cause: Throwable)
  object Cleanup

  case class NewConnection(connection: ActorRef)
  object Full
}
