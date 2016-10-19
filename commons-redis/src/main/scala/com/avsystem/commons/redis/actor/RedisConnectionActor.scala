package com.avsystem.commons
package redis.actor

import akka.actor.{Actor, ActorRef}
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import akka.util.ByteString
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.config.ConnectionConfig
import com.avsystem.commons.redis.exception._
import com.avsystem.commons.redis.protocol._
import com.avsystem.commons.redis.util.ActorLazyLogging

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.control.NonFatal

final class RedisConnectionActor(address: NodeAddress, config: ConnectionConfig, manager: ActorRef)
  extends Actor with ActorLazyLogging {

  import RedisConnectionActor._
  import context._

  private var connectionActor = Opt.empty[ActorRef]
  private var initialized = false
  private var waitingForRequests = true
  private var closeWhenIdle = false
  private var requestBeingSent = Opt.empty[SentRequest]
  private val sentRequests = new mutable.Queue[SentRequest]
  private val state = new WatchState

  private val decoder = new RedisMsg.Decoder({ replyMsg =>
    val lastRequest = sentRequests.front
    lastRequest.processMessage(replyMsg, state).foreach { packsResult =>
      sentRequests.dequeue()
      checkState()
      lastRequest.callback(packsResult)
    }
  })

  log.debug(s"Connecting to $address")
  IO(Tcp) ! Connect(address.socketAddress, config.localAddress.toOption, config.socketOptions, config.socketTimeout.toOption)

  def handleRequest(connection: ActorRef, packs: RawCommandPacks, callback: PacksResult => Unit): Unit =
    if (waitingForRequests) {
      waitingForRequests = false
      val sendBuffer = new ArrayBuffer[ArrayMsg[BulkStringMsg]]
      val requestToSend = new SentRequest(callback)
      packs.emitCommandPacks { pack =>
        val sizeBefore = sendBuffer.size
        pack.rawCommands(inTransaction = false).emitCommands(rc => sendBuffer += rc.encoded)
        requestToSend.pushPreprocessor(pack.createPreprocessor(sendBuffer.size - sizeBefore))
      }
      if (sendBuffer.isEmpty) {
        log.debug(s"Empty packs received")
        checkState()
        requestToSend.callback(PacksResult.Empty)
      } else {
        requestBeingSent = requestToSend.opt
        val encoded = RedisMsg.encode(sendBuffer)
        log.debug(s"$address >>>>\n${RedisMsg.escape(encoded, quote = false).replaceAllLiterally("\\n", "\\n\n")}")
        config.debugListener.onSend(encoded)
        connection ! Write(encoded, WriteAck)
      }
    } else {
      callback(PacksResult.Failure(new ConnectionBusyException(address)))
    }

  def receive = connecting

  def connecting: Receive = {
    case Connected(_, _) =>
      val connection = sender()
      connection ! Register(self)
      connectionActor = connection.opt
      become(connected(connection))
      log.debug(s"Connected to Redis at $address")
      handleRequest(connection, config.initCommands.rawCommandPacks,
        pr => try {
          config.initCommands.decodeReplies(pr)
          log.debug(s"Successfully initialized connection to Redis at $address")
          initialized = true
          manager ! Initialized
        } catch {
          case NonFatal(rawCause) =>
            log.error(s"Failed to initialize connection to Redis at $address", rawCause)
            val cause = new ConnectionInitializationFailure(rawCause)
            manager ! InitializationFailure(cause)
            close(cause)
        }
      )
    case CommandFailed(_: Connect) =>
      log.error(s"Connection attempt to Redis at $address failed")
      close(new ConnectionFailedException(address))
    case packs: RawCommandPacks =>
      sender() ! PacksResult.Failure(new NotYetConnectedException(address))
    case Close =>
      close(new ConnectionClosedException(address))
  }

  def connected(connection: ActorRef): Receive = {
    case packs: RawCommandPacks =>
      val s = sender()
      if (initialized) {
        handleRequest(connection, packs, s ! _)
      } else {
        s ! PacksResult.Failure(new ConnectionNotYetInitializedException(address))
      }
    case ResetState =>
      if (state.watching) {
        log.debug(s"Resetting state of connection to Redis at $address")
        handleRequest(connection, RedisApi.Batches.BinaryTyped.unwatch.rawCommandPacks,
          pr => try RedisApi.Batches.BinaryTyped.unwatch.decodeReplies(pr) catch {
            case NonFatal(cause) => failUnfinishedAndClose(new ConnectionStateResetFailure(cause))
          }
        )
      } else {
        handleRequest(connection, RedisBatch.unit.rawCommandPacks, _ => ())
      }
    case WriteAck =>
      requestBeingSent.foreach(sentRequests += _)
      requestBeingSent = Opt.Empty
      checkState()
    case CommandFailed(_: Write) =>
      log.error(s"Write to Redis at $address failed")
      requestBeingSent.foreach(_.callback(PacksResult.Failure(new WriteFailedException(address))))
      requestBeingSent = Opt.Empty
      checkState()
    case Received(data) =>
      log.debug(s"$address <<<<\n${RedisMsg.escape(data, quote = false).replaceAllLiterally("\\r\\n", "\\r\\n\n")}")
      config.debugListener.onReceive(data)
      decoder.decodeMore(data)
    case _: ConnectionClosed =>
      log.info(s"Connection to Redis at $address closed")
      connectionActor = Opt.Empty
      failUnfinishedAndClose(new ConnectionClosedException(address))
    case Close =>
      closeWhenIdle = true
      checkState()
  }

  def checkState(): Unit = {
    if (!waitingForRequests && requestBeingSent.isEmpty && config.maxSentRequests.forall(_ > sentRequests.size)) {
      if (initialized) {
        manager ! Accepted
      }
      waitingForRequests = true
    }
    if (closeWhenIdle && requestBeingSent.isEmpty && sentRequests.isEmpty) {
      connectionActor.foreach(_ ! Tcp.Close)
    }
  }

  def closed(cause: RedisException): Receive = {
    case Stop =>
      context.stop(self)
    case _: ConnectionClosed if connectionActor.contains(sender()) =>
      log.info(s"Connection to Redis at $address closed")
      connectionActor = Opt.Empty
      manager ! Closed
    case packs: RawCommandPacks =>
      sender() ! PacksResult.Failure(cause)
  }

  def close(cause: RedisException): Unit = {
    connectionActor match {
      case Opt(connection) => connection ! Tcp.Close
      case Opt.Empty => manager ! Closed
    }
    become(closed(cause))
  }

  def failUnfinished(cause: Throwable): Unit = {
    val failure = PacksResult.Failure(cause)
    log.debug(s"Failing ${sentRequests.size} sent requests and ${requestBeingSent.size} not confirmed yet")
    while (sentRequests.nonEmpty) {
      sentRequests.dequeue().callback(failure)
    }
    requestBeingSent.foreach(_.callback(failure))
    requestBeingSent = Opt.Empty
  }

  def failUnfinishedAndClose(cause: RedisException): Unit = {
    failUnfinished(cause)
    close(cause)
  }

  override def postStop() =
    failUnfinished(new ClientStoppedException(address))
}

object RedisConnectionActor {
  private object WriteAck extends Event
  private case class QueuedRequest(packs: RawCommandPacks, callback: PacksResult => Unit, blocking: Boolean = false)

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

  /**
    * Message sent back to manager actor to indicate that connection has been successfully established and initialized
    * and connection actor is ready to accept requests.
    */
  case object Initialized
  case class InitializationFailure(cause: ConnectionInitializationFailure)
  /**
    * Message sent to manager actor after connection is closed.
    */
  case object Closed
  /**
    * Message sent back to manager actor to indicate that previous request was written (successfully or not) and
    * connection actor is ready to accept another request.
    */
  case object Accepted

  case object ResetState
  case object Close
  case object Stop

  trait DebugListener {
    def onSend(data: ByteString): Unit
    def onReceive(data: ByteString): Unit
  }
  object DevNullListener extends DebugListener {
    def onSend(data: ByteString) = ()
    def onReceive(data: ByteString) = ()
  }
}
