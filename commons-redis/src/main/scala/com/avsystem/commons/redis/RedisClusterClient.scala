package com.avsystem.commons
package redis

import java.io.Closeable

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import com.avsystem.commons.collection.CollectionAliases.{BMap, MHashMap}
import com.avsystem.commons.jiop.JavaInterop._
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.RawCommand.Level
import com.avsystem.commons.redis.RedisClusterClient.{AskingPack, CollectionPacks, RedirectionError}
import com.avsystem.commons.redis.actor.ClusterMonitoringActor
import com.avsystem.commons.redis.actor.ClusterMonitoringActor.{GetClient, GetClientResponse, Refresh}
import com.avsystem.commons.redis.actor.RedisConnectionActor.PacksResult
import com.avsystem.commons.redis.commands.{Asking, SlotRange}
import com.avsystem.commons.redis.config.ClusterConfig
import com.avsystem.commons.redis.exception._
import com.avsystem.commons.redis.protocol.{ErrorMsg, FailureReply, RedisMsg, RedisReply, TransactionReply}
import com.avsystem.commons.redis.util.SingletonSeq

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.control.NonFatal

/**
  * Author: ghik
  * Created: 13/06/16.
  */
final class RedisClusterClient(
  val seedNodes: Seq[NodeAddress] = List(NodeAddress.Default),
  val clusterConfig: ClusterConfig = ClusterConfig())
  (implicit system: ActorSystem) extends Closeable {

  @volatile private[this] var state: ClusterState = ClusterState(IndexedSeq.empty, Map.empty)
  @volatile private[this] var stateListener: ClusterState => Unit = s => ()
  // clients for nodes mentioned in redirection messages which are not yet in the cluster state
  @volatile private[this] var temporaryClients: List[RedisNodeClient] = Nil
  private val initPromise = Promise[Unit]()

  // invoked by monitoring actor, effectively serialized
  private def onNewState(newState: ClusterState): Unit = {
    state = newState
    temporaryClients = Nil
    stateListener(state)
    initPromise.trySuccess(())
  }

  // invoked by monitoring actor, effectively serialized
  private def onTemporaryClient(client: RedisNodeClient): Unit = {
    temporaryClients = client :: temporaryClients
  }

  private val monitoringActor =
    system.actorOf(Props(new ClusterMonitoringActor(seedNodes, clusterConfig, onNewState, onTemporaryClient)))

  private def determineSlot(pack: RawCommandPack): Int = {
    var slot = -1
    pack.rawCommands(inTransaction = false)
      .emitCommands(_.encoded.elements.foreach { bs =>
        if (bs.isCommandKey) {
          val s = Hash.slot(bs.string)
          if (slot == -1) {
            slot = s
          } else if (s != slot) {
            throw new CrossSlotException
          }
        }
      })
    slot match {
      case -1 => throw new NoKeysException
      case _ => slot
    }
  }

  import system.dispatcher

  def setStateListener(listener: ClusterState => Unit)(implicit executor: ExecutionContext): Unit =
    stateListener = state => executor.execute(jRunnable(listener(state)))

  def currentState: ClusterState =
    state

  def initialized: Future[this.type] =
    initPromise.future.map(_ => this)

  def toExecutor(implicit timeout: Timeout): RedisClusteredExecutor =
    new RedisClusteredExecutor {
      def execute[A](batch: RedisBatch[A]) = executeBatch(batch)
    }

  private def handleRedirection(pack: RawCommandPack, result: Future[RedisReply], retryCount: Int)
    (implicit timeout: Timeout): Future[RedisReply] =

    result.flatMapNow {
      case RedirectionError(redirection) =>
        retryRedirected(pack, redirection, retryCount)
      case TransactionReply(elements) =>
        def collectRedirection(acc: Opt[Redirection], idx: Int): Opt[Redirection] =
          if (idx >= elements.size) acc
          else elements(idx) match {
            case RedirectionError(redirection) if acc.forall(_ == redirection) =>
              collectRedirection(redirection.opt, idx + 1)
            case err: ErrorMsg if err.errorCode == "EXECABORT" =>
              collectRedirection(acc, idx + 1)
            case _ => Opt.Empty
          }
        collectRedirection(Opt.Empty, 0)
          .map(red => retryRedirected(pack, red, retryCount))
          .getOrElse(result)
      case _ =>
        result
    }

  private def retryRedirected(pack: RawCommandPack, redirection: Redirection, retryCount: Int)
    (implicit timeout: Timeout): Future[RedisReply] = {

    if (!redirection.ask) {
      // force state refresh after non-ASK redirection
      monitoringActor ! Refresh(new SingletonSeq(redirection.address).opt)
    }
    val packToResend = if (redirection.ask) new AskingPack(pack) else pack
    if (retryCount >= clusterConfig.maxRedirections)
      Future.successful(FailureReply(new TooManyRedirectionsException(redirection)))
    else {
      val readyClient = state.masters.get(redirection.address) orElse
        temporaryClients.find(_.address == redirection.address)
      val result = readyClient match {
        case Some(client) =>
          client.executeRaw(packToResend)
        case None =>
          // this should only happen when a new master appears and we don't yet have a client for it
          monitoringActor.ask(GetClient(redirection.address))(RedisClusterClient.GetClientTimeout)
            .mapTo[GetClientResponse].flatMapNow(_.client.executeRaw(packToResend))
      }
      handleRedirection(pack, result.map(_.apply(0)), retryCount + 1)
    }
  }

  def executeBatch[A](batch: RedisBatch[A])(implicit timeout: Timeout): Future[A] = {
    batch.rawCommandPacks.requireLevel(Level.Node, "ClusterClient")
    initPromise.future.flatMap { _ =>
      //TODO: optimize when there's only one pack or all target the same node
      val currentState = state
      val barrier = Promise[Unit]()
      val packsByNode = new MHashMap[RedisNodeClient, ArrayBuffer[RawCommandPack]]
      val resultsByNode = new MHashMap[RedisNodeClient, Future[PacksResult]]

      def futureForPack(client: RedisNodeClient, pack: RawCommandPack): Future[RedisReply] = {
        val packBuffer = packsByNode.getOrElseUpdate(client, new ArrayBuffer)
        val idx = packBuffer.size
        packBuffer += pack
        val result = resultsByNode.getOrElseUpdate(client,
          barrier.future.flatMapNow { _ =>
            client.executeRaw(CollectionPacks(packBuffer))
          }
        ).mapNow(_.apply(idx))
        handleRedirection(pack, result, 0)
      }

      val results = new ArrayBuffer[Future[RedisReply]]
      batch.rawCommandPacks.emitCommandPacks { pack =>
        val resultFuture = try {
          val client = currentState.clientForSlot(determineSlot(pack))
          futureForPack(client, pack)
        } catch {
          case re: RedisException => Future.successful(FailureReply(re))
          case NonFatal(cause) => Future.failed(cause)
        }
        results += resultFuture
      }
      barrier.success(())
      Future.sequence(results).map(replies => batch.decodeReplies(replies))
    }
  }

  def close(): Unit = {
    system.stop(monitoringActor)
  }
}

private object RedisClusterClient {
  val GetClientTimeout = Timeout(1.seconds)

  case class CollectionPacks(coll: Traversable[RawCommandPack]) extends RawCommandPacks {
    def emitCommandPacks(consumer: RawCommandPack => Unit) = coll.foreach(consumer)
  }

  final class AskingPack(pack: RawCommandPack) extends RawCommandPack {
    def rawCommands(inTransaction: Boolean) =
      if (inTransaction) pack.rawCommands(inTransaction)
      else new RawCommands {
        def emitCommands(consumer: RawCommand => Unit) = {
          consumer(Asking)
          pack.rawCommands(inTransaction).emitCommands(consumer)
        }
      }

    def createPreprocessor(replyCount: Int) = new ReplyPreprocessor {
      private val wrapped = pack.createPreprocessor(replyCount - 1)
      private var first = true
      private var error: Opt[FailureReply] = Opt.Empty

      def preprocess(message: RedisMsg, connectionState: WatchState) =
        if (first) {
          first = false
          message match {
            case RedisMsg.Ok =>
            case _ => error = FailureReply(new UnexpectedReplyException(s"Unexpected reply for ASKING: $message")).opt
          }
          Opt.Empty
        } else wrapped.preprocess(message, connectionState)
          .map(reply => error.getOrElse(reply))
    }

    def checkLevel(minAllowed: Level, clientType: String) =
      pack.checkLevel(minAllowed, clientType)
  }

  object RedirectionError {
    def unapply(failure: ErrorMsg): Opt[Redirection] = {
      val message = failure.errorString.utf8String
      val moved = message.startsWith("MOVED ")
      val ask = message.startsWith("ASK ")
      if (moved || ask) {
        val Array(_, slot, ipport) = message.split(' ')
        val Array(ip, port) = ipport.split(':')
        Opt(Redirection(NodeAddress(ip, port.toInt), slot.toInt, ask))
      } else Opt.Empty
    }
  }
}

case class Redirection(address: NodeAddress, slot: Int, ask: Boolean)

case class ClusterState(mapping: IndexedSeq[(SlotRange, RedisNodeClient)], masters: BMap[NodeAddress, RedisNodeClient]) {
  def clientForKey(key: ByteString): RedisNodeClient =
    clientForSlot(Hash.slot(key))

  def clientForSlot(slot: Int): RedisNodeClient = {
    def binsearch(from: Int, to: Int): RedisNodeClient =
      if (from >= to) throw new UnmappedSlotException(slot)
      else {
        val mid = (from + to) / 2
        val (range, client) = mapping(mid)
        if (range.contains(slot)) client
        else if (range.start > slot) binsearch(from, mid)
        else binsearch(mid + 1, to)
      }
    binsearch(0, mapping.length)
  }
}
