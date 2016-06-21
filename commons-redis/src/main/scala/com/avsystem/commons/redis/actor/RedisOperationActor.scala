package com.avsystem.commons
package redis.actor

import akka.actor.{Actor, ActorRef}
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.RedisOp
import com.avsystem.commons.redis.RedisOp.{FlatMappedOp, LeafOp}
import com.avsystem.commons.redis.actor.RedisOperationActor.{OpFailure, OpSuccess}
import com.avsystem.commons.redis.exception.RedisException
import com.avsystem.commons.redis.util.ActorLazyLogging

import scala.util.control.NonFatal

/**
  * Implements execution of [[RedisOp]] (sequence of redis operations).
  * Separate [[RedisOperationActor]] is spawned of every [[RedisOp]] and lives only to
  * execute that single [[RedisOp]].
  *
  * Author: ghik
  * Created: 11/04/16.
  */
final class RedisOperationActor(managedConnection: ActorRef) extends Actor with ActorLazyLogging {

  context.watch(managedConnection)

  private var listener: ActorRef = null
  private var reserved = false

  def handleOperation(op: RedisOp[Any]): Unit = op match {
    case LeafOp(batch) =>
      managedConnection ! batch
      context.become(waitingForResponse(Opt.Empty))
    case FlatMappedOp(batch, nextStep) =>
      managedConnection ! batch
      context.become(waitingForResponse(Opt(nextStep)))
  }

  def waitingForReserve(op: RedisOp[Any]): Receive = {
    case ManagedRedisConnectionActor.ReserveSuccess =>
      reserved = true
      handleOperation(op)
    case ManagedRedisConnectionActor.ReserveFailure(cause) =>
      respond(OpFailure(cause))
  }

  def waitingForResponse[A, B](nextStep: Opt[A => RedisOp[B]]): Receive = {
    case RedisConnectionActor.BatchSuccess(a: A@unchecked) =>
      try {
        nextStep match {
          case Opt.Empty =>
            respond(OpSuccess(a))
          case Opt(fun) =>
            handleOperation(fun(a))
        }
      } catch {
        case NonFatal(t) => respond(OpFailure(t))
      }
    case RedisConnectionActor.BatchFailure(t) =>
      respond(OpFailure(t))
  }

  def respond(msg: Any): Unit =
    if (listener != null) {
      log.debug(s"Responding with final result: $msg")
      listener ! msg
      listener = null
      releaseConnection()
      context.stop(self)
    }

  def receive = {
    case op: RedisOp[Any] if listener == null =>
      listener = sender()
      managedConnection ! ManagedRedisConnectionActor.Reserve
      context.become(waitingForReserve(op))
  }

  def releaseConnection(): Unit =
    if (reserved) {
      reserved = false
      managedConnection ! ManagedRedisConnectionActor.Release
    }

  // nullguard redundant, but avoids unnecessary exception creation
  override def postStop() =
    if (listener != null) {
      respond(OpFailure(new RedisException("Operation killed before finishing")))
    }
}

object RedisOperationActor {
  sealed trait OpResult[+A] {
    def get: A = this match {
      case OpSuccess(result) => result
      case OpFailure(cause) => throw cause
    }
  }
  case class OpSuccess[+A](result: A) extends OpResult[A]
  case class OpFailure(cause: Throwable) extends OpResult[Nothing]
}
