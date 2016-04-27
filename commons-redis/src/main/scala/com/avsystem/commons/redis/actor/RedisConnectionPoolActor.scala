package com.avsystem.commons
package redis.actor

import akka.actor.{Actor, ActorRef, Props, Terminated}
import com.avsystem.commons.jiop.JavaInterop._
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.actor.RedisConnectionPoolActor.QueuedOp
import com.avsystem.commons.redis.util.ActorLazyLogging
import com.avsystem.commons.redis.{NodeAddress, RedisOp}

import scala.collection.mutable

/**
  * Author: ghik
  * Created: 12/04/16.
  */
final class RedisConnectionPoolActor(address: NodeAddress, size: Int) extends Actor with ActorLazyLogging {
  private val availableConnections = new JArrayDeque[ActorRef](size)
  private val deadConnections = new mutable.HashSet[ActorRef]

  private val queuedOps = new mutable.Queue[QueuedOp]

  (0 until size).foreach { _ =>
    availableConnections.add(newConnection)
  }

  def newConnection = {
    val res = context.actorOf(Props(new RedisConnectionActor(address)))
    context.watch(res) // deliberately not relying on supervision and restarting
    res
  }

  def receive = {
    case op: RedisOp[Any] =>
      queuedOps += QueuedOp(sender(), op)
      handleNextOp()
    case RedisOperationActor.Release(connection) =>
      log.debug(s"Operation finished, releasing connection $connection")
      availableConnections.add(connection)
      handleNextOp()
    case Terminated(childRef) =>
      log.debug(s"Connection $childRef died")
      deadConnections += childRef
  }

  def handleNextOp(): Unit = if (queuedOps.nonEmpty) {
    nextAvailableConnection().foreach { connection =>
      val QueuedOp(listener, op) = queuedOps.dequeue()
      val operationActor = context.actorOf(Props(new RedisOperationActor(connection, address)))
      log.debug(s"Forwarding $op to $operationActor using connection $connection")
      operationActor.tell(op, listener)
    }
  }

  def nextAvailableConnection(): Opt[ActorRef] =
    availableConnections.poll().opt match {
      case Opt.Empty =>
        log.debug("No available connections now")
        Opt.Empty
      case res@Opt(connection) =>
        if (deadConnections.remove(connection)) {
          log.debug(s"Connection $connection is dead, spawning new")
          availableConnections.add(newConnection)
          nextAvailableConnection()
        } else {
          log.debug(s"Obtained connection $connection")
          res
        }
    }
}

object RedisConnectionPoolActor {
  case class QueuedOp(listener: ActorRef, op: RedisOp[Any])
}
