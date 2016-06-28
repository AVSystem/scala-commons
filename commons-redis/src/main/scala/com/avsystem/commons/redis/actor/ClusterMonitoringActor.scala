package com.avsystem.commons
package redis.actor

import akka.actor.{Actor, Props}
import com.avsystem.commons.collection.CollectionAliases.MHashMap
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.actor.RedisConnectionActor.BatchSuccess
import com.avsystem.commons.redis.commands.{NodeInfo, SlotRange, SlotRangeMapping}
import com.avsystem.commons.redis.config.ClusterConfig
import com.avsystem.commons.redis.util.ActorLazyLogging
import com.avsystem.commons.redis.{NodeAddress, RedisCommands, RedisNodeClient}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.util.Random

final class ClusterMonitoringActor(
  seedNodes: Seq[NodeAddress],
  config: ClusterConfig,
  listener: Array[(SlotRange, RedisNodeClient)] => Any)
  extends Actor with ActorLazyLogging {

  import ClusterMonitoringActor._
  import context._

  def createConnection(addr: NodeAddress) =
    actorOf(Props(new ManagedRedisConnectionActor(addr, config.monitoringConnectionConfigs(addr))))

  def createClient(addr: NodeAddress) =
    new RedisNodeClient(addr, config.nodeConfigs(addr))

  private val random = new Random
  private var epoch = -1L
  private var masters = Seq.empty[NodeAddress]
  private val connections = MHashMap(seedNodes.map(addr => (addr, createConnection(addr))): _*)
  private val clients = new MHashMap[NodeAddress, RedisNodeClient]
  private var suspendUntil = Deadline.now

  self ! Refresh(Opt(seedNodes))
  system.scheduler.schedule(config.autoRefreshInterval, config.autoRefreshInterval, self, Refresh())

  def randomMasters(): Seq[NodeAddress] = {
    val pool = mutable.ArraySeq(masters: _*)
    val count = config.nodesToQueryForState(pool.length)
    var i = 0
    while (i < count) {
      val idx = i + random.nextInt(pool.length - i)
      val node = pool(idx)
      pool(idx) = pool(i)
      pool(i) = node
      i += 1
    }
    pool.slice(0, count)
  }

  def receive = {
    case Refresh(nodesOpt) =>
      if (suspendUntil.isOverdue) {
        nodesOpt.getOrElse(randomMasters()).foreach { node =>
          connections.getOrElseUpdate(node, createConnection(node)) ! ClusterMonitoringActor.RefreshBatch
        }
        suspendUntil = config.minRefreshInterval.fromNow
      }
    case BatchSuccess((nodes: Seq[NodeInfo@unchecked], slotRangeMapping: Seq[SlotRangeMapping@unchecked])) =>
      val newEpoch = nodes.find(_.flags.myself).map(_.configEpoch).getOrElse(-1L)
      if (newEpoch > epoch) {
        val newMasters = nodes.iterator.filter(_.flags.master).map(_.address).to[ArrayBuffer]
        log.debug(s"New cluster view received, masters: ${newMasters.mkString(", ")}")
        val newMastersSet = newMasters.toSet
        (newMastersSet diff connections.keySet).foreach { addr =>
          connections(addr) = createConnection(addr)
        }
        (newMastersSet diff clients.keySet).foreach { addr =>
          clients(addr) = createClient(addr)
        }

        val mapping = slotRangeMapping.iterator.map(srm => (srm.range, clients(srm.master))).toArray
        java.util.Arrays.sort(mapping, MappingComparator)
        listener(mapping)

        (connections.keySet diff newMastersSet).foreach { addr =>
          connections.remove(addr).foreach(context.stop)
          clients.remove(addr).foreach { client =>
            context.system.scheduler.scheduleOnce(config.nodeClientCloseDelay)(client.close())
          }
        }

        epoch = newEpoch
        masters = newMasters
      }
    case GetClient(addr) =>
      val client = clients.getOrElseUpdate(addr, createClient(addr))
      sender() ! GetClientResponse(client)
  }

  override def postStop() = {
    clients.values.foreach(_.close())
  }
}

object ClusterMonitoringActor {
  val MappingComparator = Ordering.by[(SlotRange, RedisNodeClient), Int](_._1.start)

  val RefreshBatch = {
    import RedisCommands._
    clusterNodes zip clusterSlots
  }

  case class Refresh(fromNodes: Opt[Seq[NodeAddress]] = Opt.Empty)
  case class GetClient(addr: NodeAddress)
  case class GetClientResponse(client: RedisNodeClient)
}
