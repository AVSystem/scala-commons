package com.avsystem.commons
package redis.actor

import akka.actor.{Actor, ActorRef}
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.RedisOp.{FlatMappedOp, LeafOp}
import com.avsystem.commons.redis.actor.ManagedRedisConnectionActor.Reserving
import com.avsystem.commons.redis.actor.RedisOperationActor.{OpFailure, OpSuccess}
import com.avsystem.commons.redis.exception.RedisException
import com.avsystem.commons.redis.util.ActorLazyLogging
import com.avsystem.commons.redis.{OperationBatch, RedisOp}

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

  private var listener: ActorRef = _

  def handleOperation(op: RedisOp[Any], reserving: Boolean = false): Unit = op match {
    case LeafOp(batch) =>
      val packs = batch.rawCommandPacks
      managedConnection ! (if (reserving) Reserving(packs) else packs)
      context.become(waitingForResponse(batch, Opt.Empty))
    case FlatMappedOp(batch, nextStep) =>
      val packs = batch.rawCommandPacks
      managedConnection ! (if (reserving) Reserving(packs) else packs)
      context.become(waitingForResponse(batch, Opt(nextStep)))
  }

  def waitingForResponse[A, B](prevBatch: OperationBatch[A], nextStep: Opt[A => RedisOp[B]], reserving: Boolean = false): Receive = {
    case pr: RedisConnectionActor.PacksResult =>
      try {
        val a = prevBatch.decodeReplies(pr)
        nextStep match {
          case Opt.Empty => respond(OpSuccess(a))
          case Opt(fun) => handleOperation(fun(a))
        }
      } catch {
        case NonFatal(t) => respond(OpFailure(t))
      }
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
      handleOperation(op, reserving = true)
  }

  def releaseConnection(): Unit =
    managedConnection ! ManagedRedisConnectionActor.Release

  override def postStop(): Unit =
    if (listener != null) {
      // nullguard redundant, but avoids unnecessary exception creation
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
