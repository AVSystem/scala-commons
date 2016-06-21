package com.avsystem.commons
package redis.actor

import akka.actor.{Actor, Props}
import com.avsystem.commons.collection.CollectionAliases.MHashMap
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.actor.ClusterMonitoringActor.ClusterState
import com.avsystem.commons.redis.commands.SlotRangeMapping
import com.avsystem.commons.redis.util.ActorLazyLogging
import com.avsystem.commons.redis.{NodeAddress, RedisCommands}

import scala.concurrent.duration.Duration

case class ClusterConfig(
  refreshInterval: Duration,
  minRefreshInterval: Duration
)

final class ClusterMonitoringActor(seedNodes: Seq[NodeAddress], listener: ClusterState => Any)
  extends Actor with ActorLazyLogging {

  def createConnection(addr: NodeAddress) =
    context.actorOf(Props(new ManagedRedisConnectionActor(addr)))

  private val connections = MHashMap(seedNodes.map(addr => (addr, createConnection(addr))): _*)


  def receive = ???
}

object ClusterMonitoringActor {
  val RefreshBatch = {
    import RedisCommands._
    clusterNodes zip clusterSlots
  }

  case class ClusterState(epoch: Long, slotMapping: Seq[SlotRangeMapping])
  case class Refresh(node: Opt[NodeAddress] = Opt.Empty)
}
