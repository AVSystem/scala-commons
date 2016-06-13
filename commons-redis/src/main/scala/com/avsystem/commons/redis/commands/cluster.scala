package com.avsystem.commons
package redis.commands

import akka.util.ByteString
import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion, Opt}
import com.avsystem.commons.redis.CommandEncoder.CommandArg
import com.avsystem.commons.redis.Scope.{Cluster, Connection, Node}
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.exception.UnexpectedReplyException
import com.avsystem.commons.redis.protocol.{ArrayMsg, BulkStringMsg, IntegerMsg, RedisMsg}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait ClusteredClusterApi extends ClusteredApiSubset {
  def clusterKeyslot(key: ByteString): Result[Int, Cluster] =
    execute(ClusterKeyslot(key))
}

trait NodeClusterApi extends ClusteredClusterApi with NodeApiSubset {
  def clusterAddslots(slots: Seq[Int]): Result[Unit, Node] =
    execute(ClusterAddslots(slots))
  def clusterCountFailureReports(nodeId: NodeId): Result[Long, Node] =
    execute(ClusterCountFailureReports(nodeId))
  def clusterCountkeysinslot(slot: Int): Result[Long, Node] =
    execute(ClusterCountkeysinslot(slot))
  def clusterDelslots(slots: Seq[Int]): Result[Unit, Node] =
    execute(ClusterDelslots(slots))
  def clusterFailover(option: Opt[FailoverOption]): Result[Unit, Node] =
    execute(ClusterFailover(option))
  def clusterForget(nodeId: NodeId): Result[Unit, Node] =
    execute(ClusterForget(nodeId))
  def clusterGetkeysinslot(slot: Int, count: Long): Result[Seq[ByteString], Node] =
    execute(ClusterGetkeysinslot(slot, count))
  def clusterInfo: Result[ClusterInfoReply, Node] =
    execute(ClusterInfo)
  def clusterMeet(address: NodeAddress): Result[Unit, Node] =
    execute(ClusterMeet(address))
  def clusterNodes: Result[Seq[NodeInfo], Node] =
    execute(ClusterNodes)
  def clusterReplicate(nodeId: NodeId): Result[Unit, Node] =
    execute(ClusterReplicate(nodeId))
  def clusterReset(hard: Boolean = false): Result[Unit, Node] =
    execute(ClusterReset(hard))
  def clusterSaveconfig: Result[Unit, Node] =
    execute(ClusterSaveconfig)
  def clusterSetConfigEpoch(configEpoch: Long): Result[Unit, Node] =
    execute(ClusterSetConfigEpoch(configEpoch))
  def clusterSetslot(slot: Int, subcommand: SetslotSubcommand): Result[Unit, Node] =
    execute(ClusterSetslot(slot, subcommand))
  def clusterSlaves(nodeId: NodeId): Result[Seq[NodeInfo], Node] =
    execute(ClusterSlaves(nodeId))
  def clusterSlots: Result[Seq[SlotRangeMapping], Node] =
    execute(ClusterSlots)
}

trait ConnectionClusterApi extends NodeClusterApi with ConnectionApiSubset {
  def readonly: Result[Unit, Connection] =
    execute(Readonly)
  def readwrite: Result[Unit, Connection] =
    execute(Readwrite)
}

case class ClusterAddslots(slots: Seq[Int]) extends RedisUnitCommand[Node] with Unkeyed {
  def encode = encoder("CLUSTER", "ADDSLOTS").add(slots).result
}

case class ClusterCountFailureReports(nodeId: NodeId) extends RedisLongCommand[Node] with Unkeyed {
  def encode = encoder("CLUSTER", "COUNT-FAILURE-REPORTS").add(nodeId.raw).result
}

case class ClusterCountkeysinslot(slot: Int) extends RedisLongCommand[Node] with Unkeyed {
  def encode = encoder("CLUSTER", "COUNTKEYSINSLOT").add(slot).result
}

case class ClusterDelslots(slots: Seq[Int]) extends RedisUnitCommand[Node] with Unkeyed {
  def encode = encoder("CLUSTER", "DELSLOTS").add(slots).result
}

case class ClusterFailover(option: Opt[FailoverOption]) extends RedisUnitCommand[Node] with Unkeyed {
  def encode = encoder("CLUSTER", "FAILOVER").add(option).result
}

case class ClusterForget(nodeId: NodeId) extends RedisUnitCommand[Node] with Unkeyed {
  def encode = encoder("CLUSTER", "FORGET").add(nodeId.raw).result
}

case class ClusterGetkeysinslot(slot: Int, count: Long) extends RedisBinarySeqCommand[Node] with Unkeyed {
  def encode = encoder("CLUSTER", "GETKEYSINSLOT").add(slot).add(count).result
}

case object ClusterInfo extends RedisCommand[ClusterInfoReply, Node] with Unkeyed {
  def encode = encoder("CLUSTER", "INFO").result
  def decodeExpected = {
    case BulkStringMsg(clusterInfo) => ClusterInfoReply.parse(clusterInfo.utf8String)
  }
}

case class ClusterKeyslot(key: ByteString) extends RedisCommand[Int, Cluster] with SimpleSingleKeyed {
  def encode = encoder("CLUSTER", "KEYSLOT").add(key).result
  def decodeExpected = {
    case IntegerMsg(value) => value.toInt
  }
}

case class ClusterMeet(address: NodeAddress) extends RedisUnitCommand[Node] with Unkeyed {
  def encode = encoder("CLUSTER", "MEET").add(address.ip).add(address.port).result
}

case object ClusterNodes extends RedisCommand[Seq[NodeInfo], Node] with Unkeyed {
  def encode = encoder("CLUSTER", "NODES").result
  def decodeExpected = {
    case BulkStringMsg(nodeInfos) =>
      nodeInfos.utf8String.split("\n").iterator.filter(_.nonEmpty).map(NodeInfo.parse).toIndexedSeq
  }
}

case class ClusterReplicate(nodeId: NodeId) extends RedisUnitCommand[Node] with Unkeyed {
  def encode = encoder("CLUSTER", "REPLICATE").add(nodeId.raw).result
}

case class ClusterReset(hard: Boolean) extends RedisUnitCommand[Node] with Unkeyed {
  def encode = encoder("CLUSTER", "RESET").addFlag("HARD", hard).result
}

case object ClusterSaveconfig extends RedisUnitCommand[Node] with Unkeyed {
  def encode = encoder("CLUSTER", "SAVECONFIG").result
}

case class ClusterSetConfigEpoch(configEpoch: Long) extends RedisUnitCommand[Node] with Unkeyed {
  def encode = encoder("CLUSTER", "SET-CONFIG-EPOCH").add(configEpoch).result
}

case class ClusterSetslot(slot: Int, subcommand: SetslotSubcommand) extends RedisUnitCommand[Node] with Unkeyed {
  def encode = encoder("CLUSTER", "SETSLOT").add(slot).add(subcommand).result
}

case class ClusterSlaves(nodeId: NodeId) extends RedisCommand[Seq[NodeInfo], Node] with Unkeyed {
  def encode = encoder("CLUSTER", "SLAVES").add(nodeId.raw).result
  def decodeExpected = {
    case BulkStringMsg(nodeInfos) =>
      nodeInfos.utf8String.split("\n").iterator.filter(_.nonEmpty).map(NodeInfo.parse).toIndexedSeq
  }
}

case object ClusterSlots extends RedisCommand[Seq[SlotRangeMapping], Node] with Unkeyed {
  def encode = encoder("CLUSTER", "SLOTS").result
  def decodeExpected = {
    case ArrayMsg(elements) => elements.map {
      case ArrayMsg(IntegerMsg(from) +: IntegerMsg(to) +: (master: RedisMsg) +: (replicas: IndexedSeq[RedisMsg])) =>
        val range = SlotRange(from.toInt, to.toInt)
        def parseNode(rr: RedisMsg) = rr match {
          case ArrayMsg(IndexedSeq(BulkStringMsg(ip), IntegerMsg(port), _*)) =>
            NodeAddress(ip.utf8String, port.toInt)
          case msg =>
            throw new UnexpectedReplyException(s"bad entry in CLUSTER SLOTS reply: $msg")
        }
        SlotRangeMapping(range, parseNode(master), replicas.map(parseNode))
      case msg =>
        throw new UnexpectedReplyException(s"bad reply for CLUSTER SLOTS: $msg")
    }
  }
}

case object Readonly extends RedisUnitCommand[Connection] with Unkeyed {
  def encode = encoder("READONLY").result
}

case object Readwrite extends RedisUnitCommand[Connection] with Unkeyed {
  def encode = encoder("READWRITE").result
}

case class NodeId(raw: String) extends AnyVal

sealed abstract class FailoverOption(val name: String) extends NamedEnum
object FailoverOption extends NamedEnumCompanion[FailoverOption] {
  case object Force extends FailoverOption("FORCE")
  case object Takeover extends FailoverOption("TAKEOVER")

  val values: List[FailoverOption] = caseObjects
}

sealed trait SetslotSubcommand
object SetslotSubcommand {
  case class Migrating(destinationNodeId: NodeId) extends SetslotSubcommand
  case class Importing(sourceNodeId: NodeId) extends SetslotSubcommand
  case object Stable extends SetslotSubcommand
  case class Node(nodeId: NodeId) extends SetslotSubcommand

  implicit val SubcommandCommandArg: CommandArg[SetslotSubcommand] =
    CommandArg((encoder, arg) => arg match {
      case Migrating(NodeId(nodeId)) => encoder.add("MIGRATING").add(nodeId)
      case Importing(NodeId(nodeId)) => encoder.add("IMPORTING").add(nodeId)
      case Stable => encoder.add("STABLE")
      case Node(NodeId(nodeId)) => encoder.add("NODE").add(nodeId)
    })
}

case class ClusterInfoReply(
  stateOk: Boolean,
  slotsAssigned: Int,
  slotsOk: Int,
  slotsPfail: Int,
  slotsFail: Int,
  knownNodes: Int,
  size: Int,
  currentEpoch: Long,
  myEpoch: Long,
  statsMessagesSent: Long,
  statsMessagesReceived: Long
) {
  override def toString =
    s"""
       |cluster_state:${if (stateOk) "ok" else "fail"}
       |cluster_slots_assigned:$slotsAssigned
       |cluster_slots_ok:$slotsOk
       |cluster_slots_pfail:$slotsPfail
       |cluster_slots_fail:$slotsFail
       |cluster_known_nodes:$knownNodes
       |cluster_size:$size
       |cluster_current_epoch:$currentEpoch
       |cluster_my_epoch:$myEpoch
       |cluster_stats_messages_sent:$statsMessagesSent
       |cluster_stats_messages_received:$statsMessagesReceived
     """.stripMargin.trim
}
object ClusterInfoReply {
  def parse(clusterInfo: String): ClusterInfoReply = {
    val rawReply = mutable.HashMap() ++ clusterInfo.split("\r\n").iterator
      .filter(_.nonEmpty).map(_.split(':') match { case Array(field, value) => (field, value) })

    ClusterInfoReply(
      rawReply("cluster_state") == "ok",
      rawReply("cluster_slots_assigned").toInt,
      rawReply("cluster_slots_ok").toInt,
      rawReply("cluster_slots_pfail").toInt,
      rawReply("cluster_slots_fail").toInt,
      rawReply("cluster_known_nodes").toInt,
      rawReply("cluster_size").toInt,
      rawReply("cluster_current_epoch").toLong,
      rawReply("cluster_my_epoch").toLong,
      rawReply("cluster_stats_messages_sent").toLong,
      rawReply("cluster_stats_messages_received").toLong
    )
  }
}

case class NodeInfo(
  id: NodeId,
  address: NodeAddress,
  flags: NodeFlags,
  master: Opt[NodeId],
  pingSent: Long,
  pongRecv: Long,
  configEpoch: Long,
  connected: Boolean,
  slots: Seq[SlotRange],
  importingSlots: Seq[(Int, NodeId)],
  migratingSlots: Seq[(Int, NodeId)]
) {
  override def toString = {
    val slotReprs = slots.iterator.map(_.toString) ++
      importingSlots.iterator.map({ case (s, n) => s"$s-<-$n" }) ++
      migratingSlots.iterator.map({ case (s, n) => s"$s->-$n" })
    val slotsRepr = if (slotReprs.hasNext) slotReprs.mkString(" ", " ", "") else ""
    s"$id $address $flags ${master.getOrElse("-")} $pingSent $pongRecv $configEpoch$slotsRepr"
  }
}

object NodeInfo {
  def parse(lineStr: String) = {
    val line = lineStr.split(' ')
    val Array(ip, portStr) = line(1).split(':')

    val slotRanges = new ArrayBuffer[SlotRange]
    val importingSlots = new ArrayBuffer[(Int, NodeId)]
    val migratingSlots = new ArrayBuffer[(Int, NodeId)]

    line.iterator.drop(8).foreach { str =>
      (str.indexOf("-<-"), str.indexOf("->-"), str.indexOf('-')) match {
        case (-1, -1, -1) =>
          val slot = str.toInt
          slotRanges += SlotRange(slot, slot)
        case (-1, -1, idx) =>
          slotRanges += SlotRange(str.take(idx).toInt, str.drop(idx + 1).toInt)
        case (idx, -1, _) =>
          importingSlots += ((str.take(idx).toInt, NodeId(str.drop(idx + 1))))
        case (-1, idx, _) =>
          migratingSlots += ((str.take(idx).toInt, NodeId(str.drop(idx + 1))))
      }
    }

    NodeInfo(
      NodeId(line(0)),
      NodeAddress(ip, portStr.toInt),
      NodeFlags(line(2)),
      Opt(line(3)).filter(_ != "-").map(NodeId),
      line(4).toLong,
      line(5).toLong,
      line(6).toLong,
      line(7) == "connected",
      slotRanges,
      importingSlots,
      migratingSlots
    )
  }
}

class NodeFlags(val raw: Int) extends AnyVal {

  import NodeFlags._

  def |(other: NodeFlags) = new NodeFlags(raw | other.raw)
  def &(other: NodeFlags) = new NodeFlags(raw & other.raw)
  def ^(other: NodeFlags) = new NodeFlags(raw ^ other.raw)
  def unary_~ : NodeFlags = new NodeFlags(~raw)

  def myself: Boolean = (this & Myself) != Noflags
  def master: Boolean = (this & Master) != Noflags
  def slave: Boolean = (this & Slave) != Noflags
  def pfail: Boolean = (this & Pfail) != Noflags
  def fail: Boolean = (this & Fail) != Noflags
  def handshake: Boolean = (this & Handshake) != Noflags
  def noaddr: Boolean = (this & Noaddr) != Noflags

  override def toString =
    if (this == Noflags) "noflags"
    else reprValuePairs.iterator
      .collect({ case (str, flags) if (this & flags) != Noflags => str })
      .mkString(",")
}

object NodeFlags {
  val Noflags = new NodeFlags(0)
  val Myself = new NodeFlags(1 << 0)
  val Master = new NodeFlags(1 << 1)
  val Slave = new NodeFlags(1 << 2)
  val Pfail = new NodeFlags(1 << 3)
  val Fail = new NodeFlags(1 << 4)
  val Handshake = new NodeFlags(1 << 5)
  val Noaddr = new NodeFlags(1 << 6)

  private val reprValuePairs = Seq(
    "myself" -> Myself,
    "master" -> Master,
    "slave" -> Slave,
    "fail?" -> Pfail,
    "fail" -> Fail,
    "handshake" -> Handshake,
    "noaddr" -> Noaddr
  )

  def apply(str: String): NodeFlags = {
    val flagSet = str.split(',').to[mutable.HashSet]
    reprValuePairs.foldLeft(Noflags) {
      case (res, (s, flags)) => if (flagSet(s)) res | flags else res
    }
  }
}

case class SlotRangeMapping(range: SlotRange, master: NodeAddress, replicas: Seq[NodeAddress])
case class SlotRange(start: Int, end: Int) {
  def toRange: Range = start to end
  def contains(slot: Int): Boolean = slot >= start && slot <= end
  override def toString = if (start == end) start.toString else s"$start-$end"
}
