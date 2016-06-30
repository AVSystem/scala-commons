package com.avsystem.commons
package redis.actor

import akka.actor.{Actor, Props}
import com.avsystem.commons.collection.CollectionAliases._
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.actor.RedisConnectionActor.{BatchFailure, BatchSuccess}
import com.avsystem.commons.redis.commands.{SlotRange, SlotRangeMapping}
import com.avsystem.commons.redis.config.ClusterConfig
import com.avsystem.commons.redis.util.ActorLazyLogging
import com.avsystem.commons.redis.{NodeAddress, RedisCommands, RedisNodeClient}

import scala.collection.mutable
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
  private var masters = mutable.LinkedHashSet.empty[NodeAddress]
  private val connections = MHashMap(seedNodes.map(addr => (addr, createConnection(addr))): _*)
  private val clients = new MHashMap[NodeAddress, RedisNodeClient]
  private var mapping = Array.empty[(SlotRange, RedisNodeClient)]
  private var suspendUntil = Deadline.now

  self ! Refresh(Opt(seedNodes))
  system.scheduler.schedule(config.autoRefreshInterval, config.autoRefreshInterval, self, Refresh())

  def randomMasters(): Seq[NodeAddress] = {
    val pool = masters.toArray
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
          connections.getOrElseUpdate(node, createConnection(node)) ! RedisCommands.clusterSlots
        }
        suspendUntil = config.minRefreshInterval.fromNow
      }
    case BatchSuccess(slotRangeMapping: Seq[SlotRangeMapping@unchecked]) =>
      val newMapping = slotRangeMapping.iterator.map { srm =>
        (srm.range, clients.getOrElseUpdate(srm.master, createClient(srm.master)))
      }.toArray
      java.util.Arrays.sort(newMapping, MappingComparator)

      masters = slotRangeMapping.iterator.map(_.master).to[mutable.LinkedHashSet]

      (masters diff connections.keySet).foreach { addr =>
        connections(addr) = createConnection(addr)
      }

      if (!(mapping sameElements newMapping)) {
        log.debug(s"New cluster slot mapping received:\n${slotRangeMapping.mkString("\n")}")
        mapping = newMapping
        listener(newMapping)
      }

      (connections.keySet diff masters).foreach { addr =>
        connections.remove(addr).foreach(context.stop)
      }
      (clients.keySet diff masters).foreach { addr =>
        clients.remove(addr).foreach { client =>
          system.scheduler.scheduleOnce(config.nodeClientCloseDelay)(client.close())
        }
      }
    case BatchFailure(cause) =>
      log.error(s"Failed to refresh cluster state", cause)
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

  case class Refresh(fromNodes: Opt[Seq[NodeAddress]] = Opt.Empty)
  case class GetClient(addr: NodeAddress)
  case class GetClientResponse(client: RedisNodeClient)
}
