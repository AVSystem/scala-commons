package com.avsystem.commons
package redis.actor

import akka.actor.{Actor, ActorRef}
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.RedisBatch.{ConnectionState, MessageBuffer, RepliesDecoder}
import com.avsystem.commons.redis.commands.Unwatch
import com.avsystem.commons.redis.exception.{ClientStoppedException, ConnectionClosedException, ConnectionFailedException, ConnectionStateResetFailure, WriteFailedException}
import com.avsystem.commons.redis.protocol.RedisMsg
import com.avsystem.commons.redis.util.ActorLazyLogging
import com.avsystem.commons.redis.{NodeAddress, RedisBatch}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal

final class RedisConnectionActor(address: NodeAddress) extends Actor with ActorLazyLogging {
  //TODO authentication, db selection, script loading, configuration

  import RedisConnectionActor._
  import context._

  IO(Tcp) ! Connect(address.socketAddress)

  private val queuedRequests = new mutable.Queue[QueuedRequest]
  private var requestBeingSent: Opt[SentRequest] = Opt.Empty
  private val sentRequests = new mutable.Queue[SentRequest]
  private val sendBuffer = new ArrayBuffer[RedisMsg]
  private val repliesBuffer = new ArrayBuffer[RedisMsg]
  private val state = new ConnectionState

  private val decoder = new RedisMsg.Decoder({ replyMsg =>
    val lastRequest = sentRequests.front
    val expectedReplies = lastRequest.batchSize
    repliesBuffer += replyMsg
    if (repliesBuffer.size >= expectedReplies) {
      decodeAndRespond(lastRequest, repliesBuffer)
      repliesBuffer.clear()
      sentRequests.dequeue()
    }
  })

  def decodeAndRespond(sentRequest: SentRequest, replies: ArrayBuffer[RedisMsg]): Unit = {
    log.debug(s"Decoding response")
    try {
      val result = sentRequest.decoder.decodeReplies(replies, 0, sentRequest.batchSize, state)
      sentRequest.listener ! BatchSuccess(result)
    } catch {
      case NonFatal(cause) => sentRequest.listener ! BatchFailure(cause)
    }
  }

  def handleQueued(connection: ActorRef): Unit =
    if (queuedRequests.nonEmpty && requestBeingSent.isEmpty) {
      val QueuedRequest(client, batch) = queuedRequests.dequeue()
      sendBuffer.clear()
      val decoder = batch.encodeCommands(new MessageBuffer(sendBuffer), inTransaction = false)
      val requestToSend = SentRequest(client, sendBuffer.size, decoder)
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
      val connection = sender()
      watch(connection)
      connection ! Register(self)
      become(connected(connection))
      log.debug(s"Connected to $address")
      handleQueued(connection)
    case CommandFailed(_: Connect) =>
      log.error(s"Connection failed on $address")
      failUnfinished(new ConnectionFailedException(address))
      stop(self)
    case batch: RedisBatch[Any, Nothing] =>
      log.debug(s"Not yet connected to $address, queueing batch")
      queuedRequests += QueuedRequest(sender(), batch)
  }

  def connected(connection: ActorRef): Receive = {
    case batch: RedisBatch[Any, Nothing] =>
      queuedRequests += QueuedRequest(sender(), batch)
      handleQueued(connection)
    case ResetState =>
      if (state.watching) {
        self ! Unwatch
      }
    case BatchSuccess(_) =>
      // response is received by connection actor itself only as a result of state resetting
      state.watching = false
    case BatchFailure(cause) =>
      failUnfinished(new ConnectionStateResetFailure(cause))
      stop(self)
    case WriteAck =>
      requestBeingSent.foreach(sentRequests += _)
      requestBeingSent = Opt.Empty
      handleQueued(connection)
    case Received(data) =>
      log.debug(s"$address <<<<\n${RedisMsg.escape(data, quote = false).replaceAllLiterally("\\r\\n", "\\r\\n\n")}")
      decoder.decodeMore(data)
    case CommandFailed(_: Write) =>
      log.error(s"Write failed on $address")
      queuedRequests.dequeue().listener ! BatchFailure(new WriteFailedException(address))
    case closed: ConnectionClosed =>
      log.info(s"Connection closed on $address")
      failUnfinished(new ConnectionClosedException(address))
      stop(self)
  }

  def failUnfinished(cause: Throwable): Unit = {
    val failure = BatchFailure(cause)
    while (sentRequests.nonEmpty) {
      sentRequests.dequeue().listener ! failure
    }
    requestBeingSent.foreach(_.listener ! failure)
    while (queuedRequests.nonEmpty) {
      queuedRequests.dequeue().listener ! failure
    }
  }

  def receive = connecting

  override def postStop() =
    failUnfinished(new ClientStoppedException(address))
}

object RedisConnectionActor {
  private object WriteAck extends Event
  private case class QueuedRequest(listener: ActorRef, batch: RedisBatch[Any, Nothing])
  private case class SentRequest(listener: ActorRef, batchSize: Int, decoder: RepliesDecoder[Any])

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
