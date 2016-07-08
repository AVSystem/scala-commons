package com.avsystem.commons
package redis

import java.io.Closeable

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.RedisBatch.{ConnectionState, MessageBuffer, RepliesDecoder}
import com.avsystem.commons.redis.Scope.Cluster
import com.avsystem.commons.redis.actor.ClusterMonitoringActor
import com.avsystem.commons.redis.commands.{Asking, SlotRange}
import com.avsystem.commons.redis.config.ClusterConfig
import com.avsystem.commons.redis.exception.{CrossSlotException, NoKeysException, RedisException, TooManyRedirectionsException, UnmappedSlotException}
import com.avsystem.commons.redis.protocol.{ArrayMsg, BulkStringMsg, ErrorMsg, RedisMsg}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}
import scala.util.control.NoStackTrace

/**
  * Author: ghik
  * Created: 13/06/16.
  */
final class RedisClusterClient(
  val seedNodes: Seq[NodeAddress] = List(NodeAddress.Default),
  val clusterConfig: ClusterConfig = ClusterConfig())
  (implicit system: ActorSystem) extends Closeable {

  import RedisClusterClient._

  @volatile private[this] var mapping = Opt.empty[Array[(SlotRange, RedisNodeClient)]]
  private val initPromise = Promise[Unit]()

  private def onNewMapping(newMapping: Array[(SlotRange, RedisNodeClient)]): Unit = {
    mapping = Opt(newMapping)
    initPromise.trySuccess(())
  }

  private val monitoringActor =
    system.actorOf(Props(new ClusterMonitoringActor(seedNodes, clusterConfig, onNewMapping)))

  private def clientForSlot(slot: Int): Opt[RedisNodeClient] =
    mapping.flatMap { m =>
      def binsearch(from: Int, to: Int): Opt[RedisNodeClient] =
        if (from >= to) Opt.Empty
        else {
          val mid = (from + to) / 2
          val (range, client) = m(mid)
          if (range.contains(slot)) Opt(client)
          else if (range.start > slot) binsearch(from, mid)
          else binsearch(mid + 1, to)
        }
      binsearch(0, m.length)
    }

  private def determineSlot(batch: RedisBatch[Any, Cluster]): Int = {
    var slot = -1
    val buf = new ArrayBuffer[ArrayMsg[BulkStringMsg]]
    batch.encodeCommands(new MessageBuffer(buf), inTransaction = false)
    buf.foreach(_.elements.foreach { bs =>
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
      case slot => slot
    }
  }

  import system.dispatcher

  def toAtomicExecutor(implicit timeout: Timeout): RedisExecutor[Cluster] =
    new RedisExecutor[Cluster] {
      def execute[A](batch: RedisBatch[A, Cluster]) = executeAtomically(batch)
    }

  def executeAtomically[A](batch: ClusterBatch[A])(implicit timeout: Timeout): Future[A] =
    initPromise.future.flatMap { _ =>
      val slot = determineSlot(batch)
      val client = clientForSlot(slot).getOrElse(throw new UnmappedSlotException(slot))
      tryExecuteBatch(batch.atomic, client, Redirection(client.address, slot, ask = false), 0)
    }

  private def tryExecuteBatch[A](batch: ClusterAtomicBatch[A], client: RedisNodeClient, redirection: Redirection, retry: Int)
    (implicit timeout: Timeout): Future[A] = {

    if (retry > clusterConfig.maxRedirections) {
      throw new TooManyRedirectionsException(redirection.address, redirection.slot, redirection.ask)
    }

    client.executeBatch(new RedirectableBatch(batch, redirection.ask)).recoverWith {
      case RedirectionException(red) =>
        if (!red.ask) {
          // force immediate cluster state refresh on non-ASK redirection
          monitoringActor ! ClusterMonitoringActor.Refresh(Opt(Seq(client.address)))
        }
        monitoringActor.ask(ClusterMonitoringActor.GetClient(red.address))(GetClientTimeout).flatMap {
          case ClusterMonitoringActor.GetClientResponse(newClient) =>
            tryExecuteBatch(batch, newClient, red, retry + 1)
        }
    }
  }

  def close(): Unit = {
    system.stop(monitoringActor)
  }
}

private object RedisClusterClient {
  val GetClientTimeout = Timeout(1.seconds)

  case class Redirection(address: NodeAddress, slot: Int, ask: Boolean)
  case class RedirectionException(redirection: Redirection) extends RedisException with NoStackTrace

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

  case class RedirectableBatch[+A](batch: ClusterAtomicBatch[A], asking: Boolean) extends RedisBatch[A, Cluster] {
    def encodeCommands(messageBuffer: MessageBuffer, inTransaction: Boolean) = {
      val askingDecoder = if (asking) Opt(Asking.encodeCommands(messageBuffer, inTransaction = false)) else Opt.Empty
      val decoder = batch.encodeCommands(messageBuffer, inTransaction = false)
      new RepliesDecoder[A] {
        def decodeReplies(replies: IndexedSeq[RedisMsg], start: Int, end: Int, state: ConnectionState) = {
          val actualStart = start + (if (asking) 1 else 0)
          askingDecoder.foreach(_.decodeReplies(replies, start, start + 1, state))
          def detectRedirection(acc: Opt[Redirection], idx: Int): Opt[Redirection] =
            if (idx >= end) acc
            else replies(idx) match {
              case RedirectionError(redirection) if acc.exists(_ != redirection) => Opt.Empty
              case _ => detectRedirection(acc, idx + 1)
            }
          detectRedirection(Opt.Empty, actualStart).foreach(r => throw new RedirectionException(r))
          decoder.decodeReplies(replies, actualStart, end, state)
        }
      }
    }
  }
}
