package com.avsystem.commons
package redis.actor

import akka.actor.{Actor, ActorRef, Cancellable}
import akka.stream.scaladsl._
import akka.stream.{CompletionStrategy, IgnoreComplete, Materializer, SystemMaterializer}
import akka.util.ByteString
import com.avsystem.commons.concurrent.RetryStrategy
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.commands.{PubSubCommand, PubSubEvent, ReplyDecoders}
import com.avsystem.commons.redis.config.ConnectionConfig
import com.avsystem.commons.redis.exception._
import com.avsystem.commons.redis.protocol.{RedisMsg, RedisReply, ValidRedisMsg}
import com.avsystem.commons.redis.util.ActorLazyLogging

import java.net.InetSocketAddress
import java.nio.{Buffer, ByteBuffer}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration.Duration

final class RedisConnectionActor(address: NodeAddress, config: ConnectionConfig)
  extends Actor with ActorLazyLogging { actor =>

  import RedisConnectionActor._
  import context._

  implicit val materializer: Materializer = SystemMaterializer(system).materializer

  private object IncomingPacks {
    def unapply(msg: Any): Opt[QueuedPacks] = msg match {
      case packs: RawCommandPacks => QueuedPacks(packs, sender().opt, reserve = false).opt
      case Reserving(packs) => QueuedPacks(packs, sender().opt, reserve = true).opt
      case _ => Opt.Empty
    }
  }

  private var mustInitiallyConnect: Boolean = _
  private var initPromise: Promise[Unit] = _

  // indicates how many times connection was restarted
  private var incarnation = 0
  private var reservedBy = Opt.empty[ActorRef]
  private var reservationIncarnation = Opt.empty[Int]

  private val queuedToReserve = new JArrayDeque[QueuedPacks]
  private val queuedToWrite = new JArrayDeque[QueuedPacks]
  private var writeBuffer = ByteBuffer.allocate(config.maxWriteSizeHint.getOrElse(0) + 1024)
  private var subscribed = Opt.empty[ActorRef]

  def receive: Receive = {
    case IncomingPacks(packs) =>
      handlePacks(packs)
    case open: Open =>
      onOpen(open)
      become(connecting(config.reconnectionStrategy, Opt.Empty))
      self ! Connect
  }

  private def onOpen(open: Open): Unit = {
    mustInitiallyConnect = open.mustInitiallyConnect
    initPromise = open.initPromise
  }

  private def handlePacks(packs: QueuedPacks): Unit = reservedBy match {
    case Opt.Empty =>
      if (packs.reserve) {
        log.debug(s"Reserving connection for ${packs.client.fold("")(_.toString)}")
        reservedBy = packs.client
      }
      queuedToWrite.addLast(packs)
    case packs.client =>
      if (reservationIncarnation.exists(_ != incarnation)) {
        // Connection was restarted during RedisOp - we must fail any subsequent requests from that RedisOp
        packs.reply(PacksResult.Failure(new ConnectionClosedException(address, Opt.Empty)))
      } else if (!queuedToWrite.isEmpty) {
        // During RedisOp, every batch must be sent separately
        packs.reply(PacksResult.Failure(new ConnectionBusyException(address)))
      } else {
        queuedToWrite.addLast(packs)
      }
    case _ =>
      queuedToReserve.addLast(packs)
  }

  private def handleRelease(): Unit = {
    log.debug(s"Releasing connection for ${reservedBy.fold("")(_.toString)}")
    reservedBy = Opt.Empty
    reservationIncarnation = Opt.Empty
    while (reservedBy.isEmpty && !queuedToReserve.isEmpty) {
      handlePacks(queuedToReserve.removeFirst())
    }
  }

  /**
    * Without separate "reconnecting" state we would not be able to drop old messages
    * (especially Connected message if arrived after ConnectionClosed because of some race conditions).
    * Without this state, we would process old ones instead of dropping them and creating a new connection.
    */
  private def reconnecting(retryStrategy: RetryStrategy): Receive = {
    case Connect =>
      become(connecting(retryStrategy, Opt.Empty))
      doConnect()
    case _: TcpEvent => // Ignore old messages
  }

  private def connecting(retryStrategy: RetryStrategy, readInitSender: Opt[ActorRef]): Receive = {
    case open: Open =>
      onOpen(open)
    case IncomingPacks(packs) =>
      handlePacks(packs)
    case Release if reservedBy.contains(sender()) =>
      handleRelease()
    case Connect =>
      log.debug(s"Connecting to $address")
      doConnect()
    case Connected(connection, remoteAddress, localAddress) =>
      log.debug(s"Connected to Redis at $address")
      //TODO: use dedicated retry strategy for initialization instead of reconnection strategy
      new ConnectedTo(connection, localAddress, remoteAddress).initialize(config.reconnectionStrategy)
      readInitSender.foreach(_ ! ReadAck)
    case _: ConnectionFailed | _: ConnectionClosed =>
      log.error(s"Connection attempt to Redis at $address failed or was closed")
      tryReconnect(retryStrategy, new ConnectionFailedException(address))
    case Close(cause, stopSelf) =>
      close(cause, stopSelf, tcpConnecting = true)
    case ReadInit =>
      // not sure if it's possible to receive ReadInit before Connected but just to be safe
      // delay replying with ReadAck until Connected is received
      become(connecting(retryStrategy, Opt(sender())))
    case _: TcpEvent => //ignore, this is from previous connection
  }

  // previously this was implemented using Akka IO, now using Akka Streams in a way that mimics Akka IO
  private def doConnect(): Unit = {
    // using Akka IO, this was implemented as:
    // IO(Tcp) ! Tcp.Connect(address.socketAddress,
    //   config.localAddress.toOption, config.socketOptions, config.connectTimeout.toOption)
    val src = Source.actorRefWithBackpressure[ByteString](
      ackMessage = WriteAck,
      completionMatcher = {
        case CloseConnection(true) => CompletionStrategy.immediately
        case CloseConnection(false) => CompletionStrategy.draining
      },
      failureMatcher = PartialFunction.empty
    )

    val conn = config.sslEngineCreator match {
      case OptArg(creator) => Tcp().outgoingConnectionWithTls(
        address.socketAddress,
        () => creator().setup(_.setUseClientMode(true)),
        config.localAddress.toOption,
        config.socketOptions,
        config.connectTimeout.getOrElse(Duration.Inf),
        config.idleTimeout.getOrElse(Duration.Inf),
        _ => Success(()),
        IgnoreComplete
      )
      case OptArg.Empty => Tcp().outgoingConnection(
        address.socketAddress,
        config.localAddress.toOption,
        config.socketOptions,
        connectTimeout = config.connectTimeout.getOrElse(Duration.Inf),
        idleTimeout = config.idleTimeout.getOrElse(Duration.Inf)
      )
    }

    val sink = Sink.actorRefWithBackpressure(
      ref = self,
      onInitMessage = ReadInit,
      ackMessage = ReadAck,
      onCompleteMessage = ConnectionClosed(Opt.Empty),
      onFailureMessage = cause => ConnectionClosed(Opt(cause))
    )

    val (actorRef, connFuture) = src.viaMat(conn)((_, _)).to(sink).run()
    connFuture.onCompleteNow {
      case Success(Tcp.OutgoingConnection(remoteAddress, localAddress)) =>
        self ! Connected(actorRef, remoteAddress, localAddress)
      case Failure(cause) =>
        self ! ConnectionFailed(cause)
    }
  }

  private def tryReconnect(retryStrategy: RetryStrategy, failureCause: => Throwable): Unit =
    if (incarnation == 0 && mustInitiallyConnect) {
      close(failureCause, stopSelf = false)
    } else retryStrategy.nextRetry match {
      case Opt((delay, nextStrategy)) =>
        if (delay > Duration.Zero) {
          log.info(s"Next reconnection attempt to $address in $delay")
        }
        become(reconnecting(nextStrategy))
        system.scheduler.scheduleOnce(delay, self, Connect)
      case Opt.Empty =>
        close(failureCause, stopSelf = false)
    }

  private final class ConnectedTo(connection: ActorRef, localAddr: InetSocketAddress, remoteAddr: InetSocketAddress)
    extends WatchState {

    private val decoder = new RedisMsg.Decoder
    private val collectors = new JArrayDeque[ReplyCollector]
    private var waitingForAck = false
    private var open = true
    private var unwatch = false

    // make sure bytecode contains reference to methods on Buffer and not ByteBuffer
    // JDK 11 overrides them in ByteBuffer which can lead to NoSuchMethodError when run on JDK 8

    private def flip(buf: Buffer): Unit = {
      buf.flip()
    }

    private def clear(buf: Buffer): Unit = {
      buf.clear()
    }

    def become(receive: Receive): Unit =
      context.become(receive unless {
        case Connected(oldConnection, _, _) if oldConnection != connection =>
          oldConnection ! CloseConnection(immediate = true)
      })

    def initialize(retryStrategy: RetryStrategy): Unit = {
      // Make sure that at least PING is sent so that LOADING errors are detected
      val initBatch = config.initCommands *> RedisApi.Batches.StringTyped.ping
      val initBuffer = ByteBuffer.allocate(initBatch.rawCommandPacks.encodedSize)
      // schedule a Cancellable RetryInit in case we do not receive a response for our request
      val scheduledRetry = system.scheduler.scheduleOnce(config.initTimeout, self, RetryInit(retryStrategy.next))
      new ReplyCollector(initBatch.rawCommandPacks, initBuffer, onInitResult(_, retryStrategy))
        .sendEmptyReplyOr { collector =>
          flip(initBuffer)
          val data = ByteString(initBuffer)
          logWrite(data)
          connection ! data
          become(initializing(collector, retryStrategy, scheduledRetry))
        }
    }

    def initializing(collector: ReplyCollector, retryStrategy: RetryStrategy, scheduledRetry: Cancellable): Receive = {
      case open: Open =>
        onOpen(open)
      case IncomingPacks(packs) =>
        handlePacks(packs)
      case Release if reservedBy.contains(sender()) =>
        handleRelease()
      case cc: ConnectionClosed =>
        scheduledRetry.cancel()
        onConnectionClosed(cc)
        tryReconnect(retryStrategy, new ConnectionClosedException(address, cc.error))
      case WriteAck =>
      case data: ByteString =>
        logReceived(data)
        scheduledRetry.cancel()
        try decoder.decodeMore(data)(collector.processMessage(_, this)) catch {
          case NonFatal(cause) =>
            // TODO: is there a possibility to NOT receive WriteAck up to this point? currently assuming that no
            onInitResult(PacksResult.Failure(cause), retryStrategy)
        } finally {
          sender() ! ReadAck
        }
      case ReadInit =>
        sender() ! ReadAck
      case RetryInit(newStrategy) =>
        scheduledRetry.cancel()
        initialize(newStrategy)
      case Close(cause, stop) =>
        scheduledRetry.cancel()
        close(cause, stop)
    }

    def onInitResult(packsResult: PacksResult, strategy: RetryStrategy): Unit =
      try {
        config.initCommands.decodeReplies(packsResult)
        log.debug(s"Successfully initialized Redis connection $localAddr->$remoteAddr")
        initPromise.trySuccess(())
        become(ready)
        writeIfPossible()
      } catch {
        // https://github.com/antirez/redis/issues/4624
        case e: ErrorReplyException if e.reply.errorCode == "LOADING" => strategy.nextRetry match {
          case Opt((delay, nextStrategy)) =>
            val delayMsg = if (delay > Duration.Zero) s" waiting $delay before" else ""
            log.warning(s"Redis is loading the dataset in memory,$delayMsg retrying initialization...")
            system.scheduler.scheduleOnce(delay, self, RetryInit(nextStrategy))
          case Opt.Empty => failInit(e)
        }
        case NonFatal(cause) => failInit(cause)
      }

    def failInit(cause: Throwable): Unit = {
      log.error(s"Failed to initialize Redis connection $localAddr->$remoteAddr", cause)
      close(new ConnectionInitializationFailure(cause), stopSelf = false)
    }

    def ready: Receive = {
      case open: Open =>
        onOpen(open)
        initPromise.success(())
      case IncomingPacks(packs) =>
        handlePacks(packs)
        writeIfPossible()
      case Release if reservedBy.contains(sender()) =>
        handleRelease()
        if (watching) {
          unwatch = true
        }
        writeIfPossible()
      case WriteAck =>
        waitingForAck = false
        writeIfPossible()
      case cc: ConnectionClosed =>
        onConnectionClosed(cc)
        val cause = new ConnectionClosedException(address, cc.error)
        failAlreadySent(cause)
        tryReconnect(config.reconnectionStrategy, cause)
      case data: ByteString =>
        onMoreData(data, sender())
      case ReadInit =>
        sender() ! ReadAck
      case Close(cause, stopSelf) =>
        failQueued(cause)
        if (!closeIfIdle(cause, stopSelf)) {
          // graceful close, wait for already sent commands to finish
          become(closing(cause, stopSelf))
        }
    }

    def closing(cause: Throwable, stopSelf: Boolean): Receive = {
      case open: Open =>
        onOpen(open)
        initPromise.success(())
        become(ready)
      case IncomingPacks(packs) =>
        packs.reply(PacksResult.Failure(cause))
      case Release => //ignore
      case WriteAck =>
        waitingForAck = false
      case cc: ConnectionClosed =>
        onConnectionClosed(cc)
        failAlreadySent(new ConnectionClosedException(address, cc.error))
        close(cause, stopSelf)
      case data: ByteString =>
        onMoreData(data, sender())
        closeIfIdle(cause, stopSelf)
      case ReadInit =>
        sender() ! ReadAck
    }

    def onMoreData(data: ByteString, sender: ActorRef): Unit = {
      logReceived(data)
      try decoder.decodeMore(data) { msg =>
        collectors.peekFirst() match {
          // no collectors registered, assuming this is a pub/sub message
          case null => onPubSubEvent(msg)
          case collector =>
            if (collector.processMessage(msg, this)) {
              collectors.removeFirst()
            }
        }
      } catch {
        case NonFatal(cause) => close(cause, stopSelf = false)
      } finally {
        sender ! ReadAck
      }
    }

    def onPubSubEvent(msg: RedisMsg): Unit =
      for {
        receiver <- subscribed
        event <- Opt(msg).collect({ case vm: ValidRedisMsg => vm }).collect(ReplyDecoders.pubSubEvent)
      } {
        receiver ! event
        event match {
          case PubSubEvent.Unsubscribe(_, 0) | PubSubEvent.Punsubscribe(_, 0) =>
            // exiting subscribed mode, move on with regular commands
            subscribed = Opt.Empty
            writeIfPossible()
          case _ =>
        }
      }

    def closeIfIdle(cause: Throwable, stopSelf: Boolean): Boolean =
      collectors.isEmpty && {
        close(cause, stopSelf)
        true
      }

    def isPubSub(packs: RawCommandPacks): Boolean = packs match {
      case _: PubSubCommand => true
      case _ => false
    }

    def writeIfPossible(): Unit =
      if (!waitingForAck && !queuedToWrite.isEmpty) {
        // Implementation of RedisOperationActor will not send more than one batch at once (before receiving response to
        // previous one). This guarantees that only the last batch in write queue can be a reserving one.
        val startingReservation = queuedToWrite.peekLast.reserve
        if (unwatch) {
          queuedToWrite.addFirst(QueuedPacks(RedisApi.Raw.BinaryTyped.unwatch, Opt.Empty, reserve = false))
          unwatch = false //TODO: what if UNWATCH fails?
        }

        var bufferSize = 0
        val it = queuedToWrite.iterator()

        @tailrec def computeBufferSize(packsCount: Int): Int =
          if (it.hasNext && config.maxWriteSizeHint.forall(_ > bufferSize)) {
            val nextPacks = it.next().packs
            if (isPubSub(nextPacks)) {
              bufferSize += nextPacks.encodedSize
              packsCount + 1 // stopping at pub/sub command
            } else if (subscribed.isEmpty) {
              bufferSize += nextPacks.encodedSize
              computeBufferSize(packsCount + 1)
            } else packsCount // don't send non-pubsub commands until unsubscribed
          } else packsCount

        var packsCount = computeBufferSize(0)

        waitingForAck = packsCount > 0
        if (writeBuffer.capacity < bufferSize) {
          writeBuffer = ByteBuffer.allocate(bufferSize)
        }

        while (packsCount > 0) {
          packsCount -= 1
          val queued = queuedToWrite.removeFirst()
          queued.packs match {
            // assuming that pub/sub commands are always sent as single commands, outside batches
            case pubsub: PubSubCommand =>
              // this may actually be UNSUBSCRIBE command but this is fine:
              // we're going to get an "unsubscribed" message from Redis and clear the `subscribed` field if necessary
              subscribed = queued.client
              // just encode, we don't expect response
              RedisMsg.encode(pubsub.encoded, writeBuffer)
            case packs =>
              new ReplyCollector(packs, writeBuffer, queued.reply)
                .sendEmptyReplyOr(collectors.addLast)
          }
        }

        if (waitingForAck) {
          if (startingReservation) {
            // We're about to write first batch in a RedisOp (transaction). Save current incarnation so that in case
            // connection is restarted we can fail any subsequent batches from that RedisOp (RedisOp must be fully
            // executed on a single Redis network connection).
            reservationIncarnation = incarnation.opt
          }
          flip(writeBuffer)
          val data = ByteString(writeBuffer)
          logWrite(data)
          connection ! data
          clear(writeBuffer)
        }
      }

    def close(cause: Throwable, stopSelf: Boolean): Unit = {
      failAlreadySent(cause)
      if (open) {
        open = false
        connection ! CloseConnection()
      }
      actor.close(cause, stopSelf)
    }

    def failAlreadySent(cause: Throwable): Unit = {
      val failure = PacksResult.Failure(cause)
      drain(collectors)(_.callback(failure))
    }

    def onConnectionClosed(cc: ConnectionClosed): Unit = {
      open = false
      incarnation += 1
      subscribed.foreach { receiver =>
        receiver ! PubSubEvent.ConnectionLost
        subscribed = Opt.Empty
      }
      log.error(s"Redis connection $localAddr->$remoteAddr was unexpectedly closed", cc.error.orNull)
    }

    def logReceived(data: ByteString): Unit = {
      log.debug(s"$localAddr <<<< $remoteAddr\n${RedisMsg.escape(data, quote = false).replace("\\n", "\\n\n")}")
      config.debugListener.onReceive(data)
    }

    def logWrite(data: ByteString): Unit = {
      log.debug(s"$localAddr >>>> $remoteAddr\n${RedisMsg.escape(data, quote = false).replace("\\n", "\\n\n")}")
      config.debugListener.onSend(data)
    }
  }

  private def drain[T](queue: JDeque[T])(fun: T => Unit): Unit =
    while (!queue.isEmpty) {
      fun(queue.removeFirst())
    }

  private def close(cause: Throwable, stopSelf: Boolean, tcpConnecting: Boolean = false): Unit = {
    failQueued(cause)
    initPromise.tryFailure(cause)
    if (stopSelf) {
      stop(self)
    } else {
      become(closed(cause, tcpConnecting))
    }
  }

  private def failQueued(cause: Throwable): Unit = {
    val failure = PacksResult.Failure(cause)
    drain(queuedToWrite)(_.reply(failure))
    drain(queuedToReserve)(_.reply(failure))
  }

  private def closed(cause: Throwable, tcpConnecting: Boolean): Receive = {
    case open: Open =>
      onOpen(open)
      incarnation = 0
      become(connecting(config.reconnectionStrategy, Opt.Empty))
      if (!tcpConnecting) {
        self ! Connect
      }
    case IncomingPacks(packs) =>
      packs.reply(PacksResult.Failure(cause))
    case Release => // ignore
    case Connected(connection, _, _) if tcpConnecting =>
      // failure may have happened while connecting, simply close the connection
      connection ! CloseConnection(immediate = true)
      become(closed(cause, tcpConnecting = false))
    case _: TcpEvent => // ignore
    case Close(_, true) =>
      stop(self)
  }
}

object RedisConnectionActor {
  final case class Open(mustInitiallyConnect: Boolean, initPromise: Promise[Unit])
  final case class Close(cause: Throwable, stop: Boolean)
  final case class Reserving(packs: RawCommandPacks)
  case object Release

  private object Connect
  private case class RetryInit(strategy: RetryStrategy)

  private sealed abstract class TcpEvent
  private object WriteAck extends TcpEvent
  private object ReadInit extends TcpEvent
  private object ReadAck extends TcpEvent

  private case class Connected(
    connection: ActorRef,
    remoteAddress: InetSocketAddress,
    localAddress: InetSocketAddress
  ) extends TcpEvent

  private case class ConnectionFailed(cause: Throwable) extends TcpEvent
  private case class ConnectionClosed(error: Opt[Throwable]) extends TcpEvent

  private case class CloseConnection(immediate: Boolean = false) extends TcpEvent

  private case class QueuedPacks(packs: RawCommandPacks, client: Opt[ActorRef], reserve: Boolean) {
    def reply(result: PacksResult)(implicit sender: ActorRef): Unit =
      client.foreach(_ ! result)
  }

  class ReplyCollector(packs: RawCommandPacks, writeBuffer: ByteBuffer, val callback: PacksResult => Unit) {
    private[this] var replies: MColBuilder[RedisReply, Array] = _
    private[this] var preprocessors: Any = _

    packs.emitCommandPacks { pack =>
      var commandCount = 0
      pack.rawCommands(inTransaction = false).emitCommands { rawCommand =>
        commandCount += 1
        RedisMsg.encode(rawCommand.encoded, writeBuffer)
      }
      pushPreprocessor(pack.createPreprocessor(commandCount))
    }

    def sendEmptyReplyOr(code: ReplyCollector => Unit): Unit =
      if (preprocessors == null) callback(PacksResult.Empty)
      else code(this)

    def processMessage(message: RedisMsg, state: WatchState): Boolean = {
      val packsResultOpt = preprocessors match {
        case null => Opt(PacksResult.Empty)
        case prep: ReplyPreprocessor =>
          prep.preprocess(message, state).map(PacksResult.Single)
        case queue: mutable.Queue[ReplyPreprocessor@unchecked] =>
          queue.front.preprocess(message, state).flatMap { preprocessedMsg =>
            if (replies == null) {
              replies = mutable.ArrayBuilder.make[RedisReply]
            }
            queue.dequeue()
            replies += preprocessedMsg
            if (queue.isEmpty) Opt(PacksResult.Multiple(IArraySeq.unsafeWrapArray(replies.result()))) else Opt.Empty
          }
      }
      packsResultOpt match {
        case Opt(result) =>
          callback(result)
          true
        case Opt.Empty =>
          false
      }
    }

    private def pushPreprocessor(preprocessor: ReplyPreprocessor): Unit =
      preprocessors match {
        case null => preprocessors = preprocessor
        case prep: ReplyPreprocessor => preprocessors = mutable.Queue(prep, preprocessor)
        case queue: mutable.Queue[ReplyPreprocessor@unchecked] => queue += preprocessor
      }
  }

  sealed abstract class PacksResult extends IIndexedSeq[RedisReply]
  object PacksResult {
    case object Empty extends PacksResult {
      def apply(idx: Int): RedisReply = throw new IndexOutOfBoundsException
      def length: Int = 0
    }
    case class Single(reply: RedisReply) extends PacksResult {
      def apply(idx: Int): RedisReply = if (idx == 0) reply else throw new IndexOutOfBoundsException
      def length: Int = 1
    }
    case class Multiple(replySeq: IndexedSeq[RedisReply]) extends PacksResult {
      def apply(idx: Int): RedisReply = replySeq(idx)
      def length: Int = replySeq.size
    }
    case class Failure(cause: Throwable) extends PacksResult {
      def apply(idx: Int): RedisReply = throw cause
      def length: Int = 0
    }
  }

  trait DebugListener {
    def onSend(data: ByteString): Unit
    def onReceive(data: ByteString): Unit
  }
  object DevNullListener extends DebugListener {
    def onSend(data: ByteString): Unit = ()
    def onReceive(data: ByteString): Unit = ()
  }
}
