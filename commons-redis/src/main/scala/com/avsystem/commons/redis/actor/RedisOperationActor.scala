package com.avsystem.commons
package redis.actor

import akka.actor.{Actor, ActorRef}
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.RawCommand.Level
import com.avsystem.commons.redis.RedisOp.{FlatMappedOp, LeafOp}
import com.avsystem.commons.redis.actor.RedisConnectionActor.{Release, Reserving}
import com.avsystem.commons.redis.actor.RedisOperationActor.{OpFailure, OpSuccess}
import com.avsystem.commons.redis.exception.RedisException
import com.avsystem.commons.redis.util.ActorLazyLogging
import com.avsystem.commons.redis.{RedisBatch, RedisOp}

import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

/**
  * Implements execution of [[RedisOp]] (sequence of redis operations).
  * Separate [[RedisOperationActor]] is spawned of every [[RedisOp]] and lives only to
  * execute that single [[RedisOp]].
  *
  * Author: ghik
  * Created: 11/04/16.
  */
final class RedisOperationActor(connection: ActorRef) extends Actor with ActorLazyLogging {

  context.watch(connection)

  private var listener: ActorRef = _

  def handleOperation(op: RedisOp[Any], reserving: Boolean = false): Unit = {
    def handle[A, B](batch: RedisBatch[A], nextStep: Opt[A => RedisOp[B]]) = {
      val packs = batch.rawCommandPacks.requireLevel(Level.Operation, "RedisOperation")
      connection ! (if (reserving) Reserving(packs) else packs)
      context.become(waitingForResponse(batch, nextStep))
    }
    op match {
      case LeafOp(batch) => handle(batch, Opt.Empty)
      case FlatMappedOp(batch, nextStep) => handle(batch, nextStep.opt)
    }
  }

  def waitingForResponse[A, B](prevBatch: RedisBatch[A], nextStep: Opt[A => RedisOp[B]], reserving: Boolean = false): Receive = {
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
      try handleOperation(op, reserving = true) catch {
        case NonFatal(t) => respond(OpFailure(t))
      }
  }

  def releaseConnection(): Unit =
    connection ! Release

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
