package com.avsystem.commons
package redis.actor

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.actor.RedisConnectionActor.PacksResult
import com.avsystem.commons.redis.commands.{NodeInfo, SlotRange, SlotRangeMapping}
import com.avsystem.commons.redis.config.ClusterConfig
import com.avsystem.commons.redis.exception.{ClusterInitializationException, ErrorReplyException}
import com.avsystem.commons.redis.util.{ActorLazyLogging, SingletonSeq}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.util.Random

final class ClusterMonitoringActor(
  seedNodes: Seq[NodeAddress],
  config: ClusterConfig,
  onClusterInitFailure: Throwable => Any,
  onNewClusterState: ClusterState => Any,
  onTemporaryClient: RedisNodeClient => Any
) extends Actor with ActorLazyLogging {

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

  private def createClient(addr: NodeAddress, clusterNode: Boolean = true) =
    new RedisNodeClient(addr, config.nodeConfigs(addr), clusterNode)

  private val random = new Random
  private var masters = MLinkedHashSet.empty[NodeAddress]
  private val connections = new MHashMap[NodeAddress, ActorRef]
  private val clients = new MHashMap[NodeAddress, RedisNodeClient]
  private var state = Opt.empty[ClusterState]
  private var suspendUntil = Deadline(Duration.Zero)
  private var fallbackToSeedsAfter = Deadline(Duration.Zero)
  private var scheduledRefresh = Opt.empty[Cancellable]
  private val seedFailures = new ArrayBuffer[Throwable]

  self ! Refresh(Opt.Empty)

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

  def receive: Receive = {
    case Refresh(nodeOpt) =>
      if (suspendUntil.isOverdue) {
        val addresses = nodeOpt.map(new SingletonSeq(_)).getOrElse {
          if (fallbackToSeedsAfter.isOverdue()) {
            if (state.isDefined) {
              log.warning(s"Could not fetch cluster state from current masters, using seed nodes")
            }
            seedNodes ++ randomMasters().filterNot(seedNodes.contains)
          } else randomMasters()
        }
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

        masters = nodeInfos.iterator.filter(n => n.flags.master && !n.flags.fail)
          .map(_.address).to[mutable.LinkedHashSet]
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

        if (scheduledRefresh.isEmpty) {
          val refreshInterval = config.autoRefreshInterval
          scheduledRefresh = system.scheduler.scheduleWithFixedDelay(refreshInterval, refreshInterval, self, Refresh(Opt.Empty)).opt
        }
        fallbackToSeedsAfter = config.refreshUsingSeedNodesAfter.fromNow

        (connections.keySet diff masters).foreach { addr =>
          connections.remove(addr).foreach(context.stop)
        }
        (clients.keySet diff mappedMasters).foreach { addr =>
          clients.remove(addr).foreach { client =>
            client.nodeRemoved()
            context.system.scheduler.scheduleOnce(config.nodeClientCloseDelay)(client.close())
          }
        }

      case Failure(err: ErrorReplyException)
        if state.isEmpty && seedNodes.size == 1 && config.fallbackToSingleNode &&
          err.errorStr == "ERR This instance has cluster support disabled" =>

        val addr = seedNodes.head
        log.info(s"$addr is a non-clustered node, falling back to regular node client")

        val client = clients.getOrElseUpdate(addr, createClient(addr, clusterNode = false))
        val newState = ClusterState.nonClustered(client)
        state = newState.opt
        onNewClusterState(newState)

        // we don't need monitoring connection for non-clustered node
        connections.values.foreach(context.stop)
        connections.clear()

      case Failure(cause) =>
        log.error("Failed to refresh cluster state", cause)
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

  override def postStop(): Unit = {
    scheduledRefresh.foreach(_.cancel())
    clients.values.foreach(_.close())
  }
}

object ClusterMonitoringActor {
  val StateRefresh: RedisBatch[(Seq[SlotRangeMapping], Seq[NodeInfo])] = {
    val api = RedisApi.Batches.BinaryTyped
    api.clusterSlots zip api.clusterNodes
  }
  val MappingComparator: Ordering[(SlotRange, RedisNodeClient)] =
    Ordering.by[(SlotRange, RedisNodeClient), Int](_._1.start)

  final case class Refresh(node: Opt[NodeAddress])
  final case class GetClient(addr: NodeAddress)
  final case class GetClientResponse(client: RedisNodeClient)
}
