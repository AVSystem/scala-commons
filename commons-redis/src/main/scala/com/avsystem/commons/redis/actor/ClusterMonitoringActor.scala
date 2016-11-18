package com.avsystem.commons
package redis.actor

import akka.actor.{Actor, ActorRef, Props}
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.actor.RedisConnectionActor.PacksResult
import com.avsystem.commons.redis.commands.{NodeInfo, SlotRange, SlotRangeMapping}
import com.avsystem.commons.redis.config.ClusterConfig
import com.avsystem.commons.redis.exception.ClusterInitializationException
import com.avsystem.commons.redis.util.ActorLazyLogging
import com.avsystem.commons.redis.{ClusterState, NodeAddress, RedisApi, RedisBatch, RedisNodeClient}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Random, Success, Try}

final class ClusterMonitoringActor(
  seedNodes: Seq[NodeAddress],
  config: ClusterConfig,
  onClusterInitFailure: Throwable => Any,
  onNewClusterState: ClusterState => Any,
  onTemporaryClient: RedisNodeClient => Any)
  extends Actor with ActorLazyLogging {

  import ClusterMonitoringActor._
  import context._

  private def createConnection(addr: NodeAddress): ActorRef =
    actorOf(Props(new RedisConnectionActor(addr, config.monitoringConnectionConfigs(addr))))

  private def getConnection(addr: NodeAddress, seed: Boolean): ActorRef =
    connections.getOrElse(addr, {
      openConnection(addr, seed)
      getConnection(addr, seed)
    })

  private def openConnection(addr: NodeAddress, seed: Boolean): Future[Unit] = {
    val initPromise = Promise[Unit]
    val connection = connections.getOrElseUpdate(addr, createConnection(addr))
    connection ! RedisConnectionActor.Open(seed, initPromise)
    initPromise.future
  }

  private def createClient(addr: NodeAddress) =
    new RedisNodeClient(addr, config.nodeConfigs(addr), clusterNode = true)

  private val random = new Random
  private var masters = mutable.LinkedHashSet.empty[NodeAddress]
  private val connections = new mutable.HashMap[NodeAddress, ActorRef]
  private val clients = new mutable.HashMap[NodeAddress, RedisNodeClient]
  private var state = Opt.empty[ClusterState]
  private var suspendUntil = Deadline.now
  private val seedFailures = new ArrayBuffer[Throwable]

  self ! Refresh(Opt(seedNodes))
  private val scheduledRefresh =
    system.scheduler.schedule(config.autoRefreshInterval, config.autoRefreshInterval, self, Refresh())

  private def randomMasters(): Seq[NodeAddress] = {
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
        val addresses = nodesOpt.getOrElse(randomMasters())
        log.debug(s"Asking ${addresses.mkString(",")} for cluster state")
        addresses.foreach { node =>
          getConnection(node, state.isEmpty) ! StateRefresh
        }
        suspendUntil = config.minRefreshInterval.fromNow
      }
    case pr: PacksResult => Try(StateRefresh.decodeReplies(pr)) match {
      case Success((slotRangeMapping, nodeInfos)) =>
        val newMapping = {
          val res = slotRangeMapping.iterator.map { srm =>
            (srm.range, clients.getOrElseUpdate(srm.master, createClient(srm.master)))
          }.toArray
          java.util.Arrays.sort(res, MappingComparator)
          res: IndexedSeq[(SlotRange, RedisNodeClient)]
        }

        masters = nodeInfos.iterator.filter(_.flags.master).map(_.address).to[mutable.LinkedHashSet]
        masters.foreach { addr =>
          openConnection(addr, seed = false)
        }

        val mappedMasters = slotRangeMapping.iterator.map(_.master).to[mutable.LinkedHashSet]

        if (state.forall(_.mapping != newMapping)) {
          log.info(s"New cluster slot mapping received:\n${slotRangeMapping.mkString("\n")}")
          val newState = ClusterState(newMapping, mappedMasters.iterator.map(m => (m, clients(m))).toMap)
          state = newState.opt
          onNewClusterState(newState)
        }

        (connections.keySet diff masters).foreach { addr =>
          connections.remove(addr).foreach(context.stop)
        }
        (clients.keySet diff mappedMasters).foreach { addr =>
          clients.remove(addr).foreach { client =>
            client.nodeRemoved()
            context.system.scheduler.scheduleOnce(config.nodeClientCloseDelay)(client.close())
          }
        }

      case Failure(cause) =>
        log.error(s"Failed to refresh cluster state", cause)
        if (state.isEmpty) {
          seedFailures += cause
          if (seedFailures.size == seedNodes.size) {
            val failure = new ClusterInitializationException(seedNodes)
            seedFailures.foreach(failure.addSuppressed)
            onClusterInitFailure(failure)
          }
        }
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
  val StateRefresh: RedisBatch[(Seq[SlotRangeMapping[NodeAddress]], Seq[NodeInfo])] = {
    val api = RedisApi.Batches.BinaryTyped
    api.clusterSlots zip api.clusterNodes
  }
  val MappingComparator = Ordering.by[(SlotRange, RedisNodeClient), Int](_._1.start)

  case class Refresh(fromNodes: Opt[Seq[NodeAddress]] = Opt.Empty)
  case class GetClient(addr: NodeAddress)
  case class GetClientResponse(client: RedisNodeClient)
}
