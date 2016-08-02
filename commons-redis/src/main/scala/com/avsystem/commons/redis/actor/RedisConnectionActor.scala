package com.avsystem.commons
package redis.actor

import akka.actor.{Actor, ActorRef}
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import akka.util.ByteString
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.commands.Unwatch
import com.avsystem.commons.redis.config.ConnectionConfig
import com.avsystem.commons.redis.exception._
import com.avsystem.commons.redis.protocol._
import com.avsystem.commons.redis.util.ActorLazyLogging
import com.avsystem.commons.redis.{WatchState, NodeAddress, RawCommandPacks, ReplyPreprocessor}

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
  private val sendBuffer = new ArrayBuffer[ArrayMsg[BulkStringMsg]]
  private val state = new WatchState

  private val decoder = new RedisMsg.Decoder({ replyMsg =>
    val lastRequest = sentRequests.front
    lastRequest.processMessage(replyMsg, state).foreach { packsResult =>
      sentRequests.dequeue()
      respondAndContinue(lastRequest, packsResult)
    }
  })

  private def respondAndContinue(request: SentRequest, result: PacksResult): Unit = {
    request.callback(result)
    if (blocked) {
      blocked = false
      handleQueued()
    }
  }

  IO(Tcp) ! Connect(address.socketAddress)

  queuedRequests += QueuedRequest(config.initCommands.rawCommandPacks, blocking = true,
    pr => try config.initCommands.decodeReplies(pr) catch {
      case NonFatal(cause) => failUnfinishedAndStop(new ConnectionInitializationFailure(cause))
    }
  )

  def handleQueued(): Unit =
    if (!blocked && queuedRequests.nonEmpty && requestBeingSent.isEmpty) {
      val QueuedRequest(packs, blocking, callback) = queuedRequests.dequeue()
      sendBuffer.clear()
      val requestToSend = new SentRequest(callback)
      packs.emitCommandPacks { pack =>
        val sizeBefore = sendBuffer.size
        pack.rawCommands(inTransaction = false).emitCommands(rc => sendBuffer += rc.encoded)
        requestToSend.pushPreprocessor(pack.createPreprocessor(sendBuffer.size - sizeBefore))
      }
      blocked = blocking
      if (sendBuffer.isEmpty) {
        log.debug(s"Empty packs received")
        respondAndContinue(requestToSend, PacksResult.Empty)
      } else {
        requestBeingSent = Opt(requestToSend)
        val encoded = RedisMsg.encode(sendBuffer)
        log.debug(s"$address >>>>\n${RedisMsg.escape(encoded, quote = false).replaceAllLiterally("\\r\\n", "\\r\\n\n")}")
        config.debugListener.onSend(encoded)
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
      failUnfinishedAndStop(new ConnectionFailedException(address))
    case packs: RawCommandPacks =>
      log.debug(s"Not yet connected to $address, queueing packs")
      val s = sender()
      queuedRequests += QueuedRequest(packs, blocking = false, s ! _)
  }

  def connected: Receive = {
    case packs: RawCommandPacks =>
      val s = sender()
      queuedRequests += QueuedRequest(packs, blocking = false, s ! _)
      handleQueued()
    case ResetState =>
      if (state.watching) {
        log.debug(s"Resetting state")
        queuedRequests += QueuedRequest(Unwatch, blocking = false,
          pr => try Unwatch.decodeReplies(pr) catch {
            case NonFatal(cause) => failUnfinishedAndStop(new ConnectionStateResetFailure(cause))
          }
        )
        handleQueued()
      }
    case WriteAck =>
      requestBeingSent.foreach(sentRequests += _)
      requestBeingSent = Opt.Empty
      handleQueued()
    case CommandFailed(_: Write) =>
      log.error(s"Write failed on $address")
      requestBeingSent.foreach(_.callback(PacksResult.Failure(new WriteFailedException(address))))
      requestBeingSent = Opt.Empty
      blocked = false
      handleQueued()
    case Received(data) =>
      log.debug(s"$address <<<<\n${RedisMsg.escape(data, quote = false).replaceAllLiterally("\\r\\n", "\\r\\n\n")}")
      config.debugListener.onReceive(data)
      decoder.decodeMore(data)
    case closed: ConnectionClosed =>
      log.info(s"Connection closed on $address")
      failUnfinishedAndStop(new ConnectionClosedException(address))
  }

  def failUnfinished(cause: Throwable): Unit = {
    val failure = PacksResult.Failure(cause)
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
  private case class QueuedRequest(packs: RawCommandPacks, blocking: Boolean, callback: PacksResult => Unit)

  private class SentRequest(val callback: PacksResult => Unit) {
    private[this] var replies: ArrayBuffer[RedisReply] = _
    private[this] var preprocessors: Any = _

    def processMessage(message: RedisMsg, state: WatchState): Opt[PacksResult] =
      preprocessors match {
        case null => Opt(PacksResult.Empty)
        case prep: ReplyPreprocessor =>
          prep.preprocess(message, state).map(PacksResult.Single)
        case queue: mutable.Queue[ReplyPreprocessor@unchecked] =>
          queue.front.preprocess(message, state).flatMap { preprocessedMsg =>
            if (replies == null) {
              replies = new ArrayBuffer(queue.length)
            }
            queue.dequeue()
            replies += preprocessedMsg
            if (queue.isEmpty) Opt(PacksResult.Multiple(replies)) else Opt.Empty
          }
      }

    def pushPreprocessor(preprocessor: ReplyPreprocessor): Unit =
      preprocessors match {
        case null => preprocessors = preprocessor
        case prep: ReplyPreprocessor => preprocessors = mutable.Queue(prep, preprocessor)
        case queue: mutable.Queue[ReplyPreprocessor@unchecked] => queue += preprocessor
      }
  }

  sealed trait PacksResult extends (Int => RedisReply)
  object PacksResult {
    case object Empty extends PacksResult {
      def apply(idx: Int) = throw new NoSuchElementException
    }
    case class Single(reply: RedisReply) extends PacksResult {
      def apply(idx: Int) = reply
    }
    case class Multiple(replySeq: IndexedSeq[RedisReply]) extends PacksResult {
      def apply(idx: Int) = replySeq(idx)
    }
    case class Failure(cause: Throwable) extends PacksResult {
      def apply(idx: Int) = throw cause
    }
  }

  case object ResetState

  trait DebugListener {
    def onSend(data: ByteString): Unit
    def onReceive(data: ByteString): Unit
  }
  object DevNullListener extends DebugListener {
    def onSend(data: ByteString) = ()
    def onReceive(data: ByteString) = ()
  }
}
