package com.avsystem.commons
package redis.actor

import akka.actor.FSM.Failure
import akka.actor.{Actor, ActorRef}
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import com.avsystem.commons.redis.NodeAddress
import com.avsystem.commons.redis.actor.RedisConnectionActor.{QueuedRequest, Request, Response, WriteAck}
import com.avsystem.commons.redis.exception.{ClientStoppedException, ConnectionClosedException, ConnectionFailedException, WriteFailedException}
import com.avsystem.commons.redis.protocol.RedisMsg
import com.avsystem.commons.redis.util.ActorLazyLogging

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Author: ghik
  * Created: 05/04/16.
  */
final class RedisConnectionActor(address: NodeAddress) extends Actor with ActorLazyLogging {
  //TODO authentication, db selection, script loading, configuration

  import context._

  IO(Tcp) ! Connect(address.socketAddress)

  private val pendingRequests = new mutable.Queue[QueuedRequest]
  private val queuedRequests = new mutable.Queue[QueuedRequest]
  private val sentRequests = new mutable.Queue[QueuedRequest]
  private var repliesBuffer: ArrayBuffer[RedisMsg] = null

  private val decoder = new RedisMsg.Decoder({ replyMsg =>
    val lastRequest = sentRequests.front
    val expectedReplies = lastRequest.request.messages.size
    if (repliesBuffer == null) {
      repliesBuffer = new ArrayBuffer[RedisMsg](expectedReplies)
    }
    repliesBuffer += replyMsg
    if (repliesBuffer.size >= expectedReplies) {
      log.debug(s"Replying with $repliesBuffer")
      lastRequest.listener ! Response(repliesBuffer)
      repliesBuffer = null
      sentRequests.dequeue()
    }
  })

  def connecting: Receive = {
    case Connected(_, _) =>
      val connection = sender()
      connection ! Register(self)
      become(connected(connection))
      log.debug(s"Connected to $address")
      while (pendingRequests.nonEmpty) {
        self ! pendingRequests.dequeue()
      }
    case CommandFailed(_: Connect) =>
      log.error(s"Connection failed on $address")
      failUnfinished(new ConnectionFailedException(address))
      stop(self)
    case req: Request =>
      log.debug(s"Not yet connected to $address, queueing request $req")
      pendingRequests += QueuedRequest(sender(), req)
  }

  def connected(connection: ActorRef): Receive = {
    case Request(messages) if messages.isEmpty =>
      log.debug(s"Empty request received, replying with empty response")
      sender() ! Response(IndexedSeq.empty)
    case req: Request =>
      self ! QueuedRequest(sender(), req)
    case qreq@QueuedRequest(client, Request(messages)) =>
      queuedRequests += qreq
      val encoded = RedisMsg.encode(messages)
      log.debug(s"$address >>>>\n${RedisMsg.escape(encoded, quote = false).replaceAllLiterally("\\r\\n", "\\r\\n\n")}")
      connection ! Write(encoded, WriteAck)
    case WriteAck =>
      sentRequests += queuedRequests.dequeue()
    case Received(data) =>
      log.debug(s"$address <<<<\n${RedisMsg.escape(data, quote = false).replaceAllLiterally("\\r\\n", "\\r\\n\n")}")
      decoder.decodeMore(data)
    case CommandFailed(_: Write) =>
      log.error(s"Write failed on $address")
      queuedRequests.dequeue().listener ! Failure(new WriteFailedException(address))
    case closed: ConnectionClosed =>
      log.info(s"Connection closed on $address")
      failUnfinished(new ConnectionClosedException(address))
      stop(self)
  }

  def failUnfinished(cause: Throwable): Unit = {
    val failure = Failure(cause)
    while (sentRequests.nonEmpty) {
      sentRequests.dequeue().listener ! failure
    }
    while (queuedRequests.nonEmpty) {
      queuedRequests.dequeue().listener ! failure
    }
    while (pendingRequests.nonEmpty) {
      pendingRequests.dequeue().listener ! failure
    }
  }

  def receive = connecting

  override def postStop() =
    failUnfinished(new ClientStoppedException(address))
}

object RedisConnectionActor {
  private object WriteAck extends Event
  private case class QueuedRequest(listener: ActorRef, request: Request)

  case class Request(messages: IndexedSeq[RedisMsg])
  case class Response(messages: IndexedSeq[RedisMsg])
  case class Failure(cause: Throwable)
}
