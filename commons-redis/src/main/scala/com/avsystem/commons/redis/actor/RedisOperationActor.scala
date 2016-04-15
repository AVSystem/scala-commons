package com.avsystem.commons
package redis.actor

import akka.actor.{Actor, ActorRef, Terminated}
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.RedisFlushable.{MessageBuffer, RepliesDecoder}
import com.avsystem.commons.redis.RedisOp.{FlatMappedOp, LeafOp}
import com.avsystem.commons.redis.actor.RedisOperationActor.{Failure, Release, Response}
import com.avsystem.commons.redis.protocol.RedisMsg
import com.avsystem.commons.redis.util.ActorLazyLogging
import com.avsystem.commons.redis.{RedisFlushable, RedisOp}

import scala.collection.mutable.ArrayBuffer
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

  private var listener: ActorRef = null
  private var released = false

  def handleFlushable[A](flushable: RedisFlushable[A]): RepliesDecoder[A] = {
    val buf = new ArrayBuffer[RedisMsg]
    val decoder = flushable.encodeCommands(new MessageBuffer(buf), inTransaction = false)
    log.debug(s"Sending $buf to connection $connection")
    connection ! RedisConnectionActor.Request(buf)
    decoder
  }

  def handleOperation(op: RedisOp[Any]): Unit = op match {
    case LeafOp(flushable) =>
      val decoder = handleFlushable(flushable)
      releaseConnection() // release the connection immediately after using it for the last time
      context.become(waitingForResponse(decoder, Opt.Empty))
    case FlatMappedOp(flushable, nextStep) =>
      val decoder = handleFlushable(flushable)
      context.become(waitingForResponse(decoder, Opt(nextStep)))
  }

  def waitingForResponse[A, B](decoder: RepliesDecoder[A], nextStep: Opt[A => RedisOp[B]]): Receive = {
    case RedisConnectionActor.Response(replies) =>
      try {
        val a = decoder.decodeReplies(replies, 0, replies.size)
        nextStep match {
          case Opt.Empty => respond(Response(a))
          case Opt(fun) => handleOperation(fun(a))
        }
      } catch {
        case NonFatal(t) => respond(Failure(t))
      }
    case RedisConnectionActor.Failure(t) =>
      respond(Failure(t))
    case Terminated(`connection`) =>
      respond(connectionTerminated)
  }

  def respond(msg: Any): Unit =
    if (listener != null) {
      log.debug(s"Responding with final result: $msg")
      listener ! msg
      listener = null
      releaseConnection()
      context.stop(self)
    }

  def connectionTerminated =
    Failure(new Exception("Connection actor terminated")) //TODO better exception

  def receive = {
    case op: RedisOp[Any] =>
      listener = sender()
      handleOperation(op)
    case Terminated(`connection`) =>
      respond(connectionTerminated)
  }

  def releaseConnection(): Unit =
    if (!released) {
      released = true
      context.parent ! Release(connection)
    }

  // nullguard redundant, but avoids unnecessary exception creation
  override def postStop() =
    if (listener != null) {
      respond(Failure(new Exception("Operation killed before finishing")))
    }
}

object RedisOperationActor {
  case class Release(connection: ActorRef)
  case class Response(result: Any)
  case class Failure(cause: Throwable)
}
