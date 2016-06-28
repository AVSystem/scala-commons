package com.avsystem.commons
package redis.actor

import akka.actor.{Actor, ActorRef}
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.RedisBatch.{ConnectionState, MessageBuffer, RepliesDecoder}
import com.avsystem.commons.redis.commands.Unwatch
import com.avsystem.commons.redis.config.ConnectionConfig
import com.avsystem.commons.redis.exception._
import com.avsystem.commons.redis.protocol.RedisMsg
import com.avsystem.commons.redis.util.ActorLazyLogging
import com.avsystem.commons.redis.{NodeAddress, RedisBatch}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal

final class RedisConnectionActor(address: NodeAddress, config: ConnectionConfig)
  extends Actor with ActorLazyLogging {

  import RedisConnectionActor._
  import context._

  private var connection: ActorRef = _
  private val queuedRequests = new mutable.Queue[QueuedRequest]
  private var requestBeingSent: Opt[SentRequest] = Opt.Empty
  private var blocked = false
  private val sentRequests = new mutable.Queue[SentRequest]
  private val sendBuffer = new ArrayBuffer[RedisMsg]
  private val repliesBuffer = new ArrayBuffer[RedisMsg]
  private val state = new ConnectionState

  private val decoder = new RedisMsg.Decoder({ replyMsg =>
    val lastRequest = sentRequests.front
    val expectedReplies = lastRequest.batchSize
    repliesBuffer += replyMsg
    if (repliesBuffer.size >= expectedReplies) {
      sentRequests.dequeue()
      decodeAndRespond(lastRequest, repliesBuffer)
      repliesBuffer.clear()
    }
  })

  IO(Tcp) ! Connect(address.socketAddress)

  queuedRequests += QueuedRequest(config.initCommands, blocking = true, {
    case BatchSuccess(_) =>
    case BatchFailure(cause) =>
      failUnfinishedAndStop(new ConnectionInitializationFailure(cause))
  })

  def decodeAndRespond(sentRequest: SentRequest, replies: ArrayBuffer[RedisMsg]): Unit = {
    log.debug(s"Decoding response")
    try {
      val result = sentRequest.decoder.decodeReplies(replies, 0, sentRequest.batchSize, state)
      sentRequest.callback(BatchSuccess(result))
    } catch {
      case NonFatal(cause) => sentRequest.callback(BatchFailure(cause))
    } finally {
      blocked = false
      handleQueued()
    }
  }

  def handleQueued(): Unit =
    if (!blocked && queuedRequests.nonEmpty && requestBeingSent.isEmpty) {
      val QueuedRequest(batch, blocking, callback) = queuedRequests.dequeue()
      sendBuffer.clear()
      val decoder = batch.encodeCommands(new MessageBuffer(sendBuffer), inTransaction = false)
      val requestToSend = SentRequest(sendBuffer.size, decoder, callback)
      blocked = blocking
      if (sendBuffer.isEmpty) {
        log.debug(s"Empty batch received")
        decodeAndRespond(requestToSend, sendBuffer)
      } else {
        requestBeingSent = Opt(requestToSend)
        val encoded = RedisMsg.encode(sendBuffer)
        log.debug(s"$address >>>>\n${RedisMsg.escape(encoded, quote = false).replaceAllLiterally("\\r\\n", "\\r\\n\n")}")
        connection ! Write(encoded, WriteAck)
      }
    }

  def connecting: Receive = {
    case Connected(_, _) =>
      connection = sender()
      watch(connection)
      connection ! Register(self)
      become(connected)
      log.debug(s"Connected to $address")
      handleQueued()
    case CommandFailed(_: Connect) =>
      log.error(s"Connection failed on $address")
      failUnfinished(new ConnectionFailedException(address))
      stop(self)
    case batch: RedisBatch[Any, Nothing] =>
      log.debug(s"Not yet connected to $address, queueing batch")
      val s = sender()
      queuedRequests += QueuedRequest(batch, blocking = false, s ! _)
  }

  def connected: Receive = {
    case batch: RedisBatch[Any, Nothing] =>
      val s = sender()
      queuedRequests += QueuedRequest(batch, blocking = false, s ! _)
      handleQueued()
    case ResetState =>
      if (state.watching) {
        queuedRequests += QueuedRequest(Unwatch, blocking = false, {
          case BatchSuccess(_) => state.watching = false
          case BatchFailure(cause) => failUnfinishedAndStop(new ConnectionStateResetFailure(cause))
        })
      }
    case WriteAck =>
      requestBeingSent.foreach(sentRequests += _)
      requestBeingSent = Opt.Empty
      handleQueued()
    case CommandFailed(_: Write) =>
      log.error(s"Write failed on $address")
      requestBeingSent.foreach(_.callback(BatchFailure(new WriteFailedException(address))))
      requestBeingSent = Opt.Empty
      blocked = false
      handleQueued()
    case Received(data) =>
      log.debug(s"$address <<<<\n${RedisMsg.escape(data, quote = false).replaceAllLiterally("\\r\\n", "\\r\\n\n")}")
      decoder.decodeMore(data)
    case closed: ConnectionClosed =>
      log.info(s"Connection closed on $address")
      failUnfinishedAndStop(new ConnectionClosedException(address))
  }

  def failUnfinished(cause: Throwable): Unit = {
    val failure = BatchFailure(cause)
    while (sentRequests.nonEmpty) {
      sentRequests.dequeue().callback(failure)
    }
    requestBeingSent.foreach(_.callback(failure))
    requestBeingSent = Opt.Empty
    while (queuedRequests.nonEmpty) {
      queuedRequests.dequeue().callback(failure)
    }
  }

  def failUnfinishedAndStop(cause: Throwable): Unit = {
    failUnfinished(cause)
    stop(self)
  }

  def receive = connecting

  override def postStop() =
    failUnfinished(new ClientStoppedException(address))
}

object RedisConnectionActor {
  private object WriteAck extends Event
  private case class QueuedRequest(batch: RedisBatch[Any, Nothing], blocking: Boolean, callback: BatchResult[Any] => Unit)
  private case class SentRequest(batchSize: Int, decoder: RepliesDecoder[Any], callback: BatchResult[Any] => Unit)

  case object ResetState

  sealed trait BatchResult[+A] {
    def get: A = this match {
      case BatchSuccess(result) => result
      case BatchFailure(cause) => throw cause
    }
  }
  case class BatchSuccess[+A](result: A) extends BatchResult[A]
  case class BatchFailure(cause: Throwable) extends BatchResult[Nothing]
}
