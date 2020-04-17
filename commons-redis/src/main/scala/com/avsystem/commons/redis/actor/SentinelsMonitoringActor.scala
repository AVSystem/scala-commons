package com.avsystem.commons
package redis.actor

import akka.actor.{Actor, ActorRef, Props}
import com.avsystem.commons.redis.actor.RedisConnectionActor.PacksResult
import com.avsystem.commons.redis.commands.{PubSubEvent, Subscribe}
import com.avsystem.commons.redis.config.MasterSlaveConfig
import com.avsystem.commons.redis.exception.MasterSlaveInitializationException
import com.avsystem.commons.redis.protocol.BulkStringMsg
import com.avsystem.commons.redis.util.ActorLazyLogging
import com.avsystem.commons.redis.{NodeAddress, RedisApi, RedisBatch, RedisNodeClient}

final class SentinelsMonitoringActor(
  masterName: String,
  seedSentinels: Seq[NodeAddress],
  config: MasterSlaveConfig,
  onInitFailure: Throwable => Unit,
  onMasterChange: RedisNodeClient => Unit
) extends Actor with ActorLazyLogging {

  import RedisApi.Batches.StringTyped._
  import SentinelsMonitoringActor._
  import context._

  private val sentinels = new MHashMap[NodeAddress, ActorRef]
  private val seedFailures = new MArrayBuffer[Throwable]
  private var master = Opt.empty[RedisNodeClient]

  private val FetchState: RedisBatch[(NodeAddress, Set[NodeAddress])] =
    sentinelGetMasterAddrByName(masterName) zip
      sentinelSentinels(masterName).map(_.flatMap(extractSentinelAddress).toSet)

  private def extractSentinelAddress(info: BMap[String, String]): Opt[NodeAddress] =
    Try(NodeAddress(info("ip"), info("port").toInt)).toOpt

  seedSentinels.foreach(getConnection(_, seed = true))

  private def getConnection(addr: NodeAddress, seed: Boolean): ActorRef =
    sentinels.getOrElse(addr, openConnection(addr, seed))

  private def openConnection(addr: NodeAddress, seed: Boolean): ActorRef = {
    log.debug(s"Opening monitoring connection to sentinel $addr")
    val conn = actorOf(Props(new RedisConnectionActor(addr, config.sentinelConnectionConfigs(addr))))
    sentinels(addr) = conn
    conn ! RedisConnectionActor.Open(seed, Promise[Unit])
    conn ! FetchState
    conn ! SentinelSubscription
    conn
  }

  private def updateMaster(masterAddr: NodeAddress): Unit =
    if (master.forall(_.address != masterAddr)) {
      log.info(s"Master node for $masterName changed to $masterAddr")
      val newMaster = new RedisNodeClient(masterAddr, config.masterConfig(masterAddr))
      onMasterChange(newMaster)
      master.foreach(_.nodeRemoved())
      master = Opt(newMaster)
    }

  def receive: Receive = {
    case pr: PacksResult => Try(FetchState.decodeReplies(pr)) match {
      case Success((masterAddr, otherSentinels)) =>
        val thisSentinel = sender()
        updateMaster(masterAddr)

        otherSentinels.foreach(getConnection(_, seed = false))
        sentinels.foreach { case (addr, conn) =>
          if (conn != thisSentinel && !otherSentinels.contains(addr)) {
            context.stop(conn)
            sentinels.remove(addr)
          }
        }

      case Failure(cause) =>
        log.error(s"Failed to fetch master node for $masterName from a sentinel", cause)
        if (master.isEmpty) {
          seedFailures += cause
          if (seedFailures.size == seedSentinels.size) {
            val failure = new MasterSlaveInitializationException(masterName, seedSentinels)
            seedFailures.foreach(failure.addSuppressed)
            onInitFailure(failure)
          }
        }
    }

    case PubSubEvent.Message(SwitchMasterChannel, BulkStringMsg(newMasterInfo)) =>
      // <master-name> <old-ip> <old-port> <new-ip> <new-port>
      val parts = newMasterInfo.utf8String.split(" ")
      if (parts(0) == masterName) {
        val masterAddr = NodeAddress(parts(3), parts(4).toInt)
        updateMaster(masterAddr)
      }

    case PubSubEvent.Message(NewSentinelChannel, BulkStringMsg(string)) =>
      // sentinel <id> <ip> <port> @ <master-name> <master-ip> <master-port>
      val parts = string.utf8String.split(" ")
      if (parts(0) == "sentinel" && parts(5) == masterName) {
        val newSentinelAddr = NodeAddress(parts(2), parts(3).toInt)
        getConnection(newSentinelAddr, seed = false)
      }

    case PubSubEvent.ConnectionLost =>
      val sentinel = sender()
      sentinel ! FetchState
      sentinel ! SentinelSubscription

    case _: PubSubEvent =>
    // ignore
  }
}
object SentinelsMonitoringActor {
  final val SwitchMasterChannel = "+switch-master"
  final val NewSentinelChannel = "+sentinel"

  private final val SentinelSubscription =
    new Subscribe(Seq(SwitchMasterChannel, NewSentinelChannel))
}
