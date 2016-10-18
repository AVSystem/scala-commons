package com.avsystem.commons
package redis.actor

import akka.actor.{Actor, Deploy, Props}
import com.avsystem.commons.collection.CollectionAliases._
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.actor.RedisConnectionActor.PacksResult
import com.avsystem.commons.redis.commands.SlotRange
import com.avsystem.commons.redis.config.ClusterConfig
import com.avsystem.commons.redis.exception.ClientStoppedException
import com.avsystem.commons.redis.util.ActorLazyLogging
import com.avsystem.commons.redis.{ClusterState, NodeAddress, RedisApi, RedisNodeClient}

import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success, Try}

final class ClusterMonitoringActor(
  seedNodes: Seq[NodeAddress],
  deploy: Deploy,
  config: ClusterConfig,
  onNewClusterState: ClusterState => Any,
  onTemporaryClient: RedisNodeClient => Any)
  extends Actor with ActorLazyLogging {

  import ClusterMonitoringActor._
  import context._

  def createConnection(addr: NodeAddress) =
    actorOf(Props(new ManagedRedisConnectionActor(addr,
      config.monitoringConnectionConfigs(addr), config.nodeConfigs(addr).reconnectionStrategy)))

  def createClient(addr: NodeAddress) =
    new RedisNodeClient(addr, config.nodeConfigs(addr), deploy)

  private val random = new Random
  private var masters = mutable.LinkedHashSet.empty[NodeAddress]
  private val connections = MHashMap(seedNodes.map(addr => (addr, createConnection(addr))): _*)
  private val clients = new MHashMap[NodeAddress, RedisNodeClient]
  private var state = ClusterState(IndexedSeq.empty, Map.empty)
  private var suspendUntil = Deadline.now

  self ! Refresh(Opt(seedNodes))
  private val scheduledRefresh =
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
          connections.getOrElseUpdate(node, createConnection(node)) ! RedisApi.Batches.BinaryTyped.clusterSlots
        }
        suspendUntil = config.minRefreshInterval.fromNow
      }
    case pr: PacksResult => Try(RedisApi.Batches.BinaryTyped.clusterSlots.decodeReplies(pr)) match {
      case Success(slotRangeMapping) =>
        val newMapping = {
          val res = slotRangeMapping.iterator.map { srm =>
            (srm.range, clients.getOrElseUpdate(srm.master, createClient(srm.master)))
          }.toArray
          java.util.Arrays.sort(res, MappingComparator)
          res: IndexedSeq[(SlotRange, RedisNodeClient)]
        }

        masters = slotRangeMapping.iterator.map(_.master).to[mutable.LinkedHashSet]

        (masters diff connections.keySet).foreach { addr =>
          connections(addr) = createConnection(addr)
        }

        if (state.mapping != newMapping) {
          log.info(s"New cluster slot mapping received:\n${slotRangeMapping.mkString("\n")}")
          state = ClusterState(newMapping, masters.iterator.map(m => (m, clients(m))).toMap)
          onNewClusterState(state)
        }

        (connections.keySet diff masters).foreach { addr =>
          connections.remove(addr).foreach(context.stop)
        }
        (clients.keySet diff masters).foreach { addr =>
          clients.remove(addr).foreach { client =>
            client.nodeRemoved()
            context.system.scheduler.scheduleOnce(config.nodeClientCloseDelay)(client.close())
          }
        }
      case Failure(_: ClientStoppedException) =>
      // node was removed, everything is normal
      case Failure(cause) =>
        log.error(s"Failed to refresh cluster state", cause)
    }
    case GetClient(addr) =>
      val client = clients.getOrElseUpdate(addr, {
        val tempClient = createClient(addr)
        onTemporaryClient(tempClient)
        tempClient
      })
      sender() ! GetClientResponse(client)
  }

  override def postStop() = {
    scheduledRefresh.cancel()
    clients.values.foreach(_.close())
  }
}

object ClusterMonitoringActor {
  val MappingComparator = Ordering.by[(SlotRange, RedisNodeClient), Int](_._1.start)

  case class Refresh(fromNodes: Opt[Seq[NodeAddress]] = Opt.Empty)
  case class GetClient(addr: NodeAddress)
  case class GetClientResponse(client: RedisNodeClient)
}
