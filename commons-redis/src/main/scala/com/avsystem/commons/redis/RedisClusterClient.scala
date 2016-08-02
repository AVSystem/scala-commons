package com.avsystem.commons
package redis

import java.io.Closeable

import akka.actor.{ActorSystem, Props}
import akka.util.{ByteString, Timeout}
import com.avsystem.commons.collection.CollectionAliases.{BMap, MHashMap}
import com.avsystem.commons.jiop.JavaInterop._
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.RawCommand.Level
import com.avsystem.commons.redis.RedisClusterClient.CollectionPacks
import com.avsystem.commons.redis.actor.ClusterMonitoringActor
import com.avsystem.commons.redis.actor.RedisConnectionActor.PacksResult
import com.avsystem.commons.redis.commands.SlotRange
import com.avsystem.commons.redis.config.ClusterConfig
import com.avsystem.commons.redis.exception._
import com.avsystem.commons.redis.protocol.{ErrorMsg, FailureReply, RedisReply}

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
  private val initPromise = Promise[Unit]()

  private def onNewState(newState: ClusterState): Unit = {
    state = newState
    stateListener(state)
    initPromise.trySuccess(())
  }

  private val monitoringActor =
    system.actorOf(Props(new ClusterMonitoringActor(seedNodes, clusterConfig, onNewState)))

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

  def toExecutor(implicit timeout: Timeout): RedisExecutor =
    new RedisExecutor {
      def execute[A](batch: RedisBatch[A]) = executeBatch(batch)
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
        resultsByNode.getOrElseUpdate(client, barrier.future.flatMapNow { _ =>
          client.executeRaw(CollectionPacks(packBuffer))
        }).mapNow(_.apply(idx))
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

  case class Redirection(address: NodeAddress, slot: Int, ask: Boolean)
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
