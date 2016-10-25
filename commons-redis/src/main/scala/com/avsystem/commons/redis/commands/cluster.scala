package com.avsystem.commons
package redis.commands

import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion, Opt, OptArg}
import com.avsystem.commons.redis.CommandEncoder.CommandArg
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.commands.ReplyDecoders._
import com.avsystem.commons.redis.protocol.{ArrayMsg, BulkStringMsg, IntegerMsg, RedisMsg}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait KeyedClusterApi extends ApiSubset {
  def keySlot(key: Key): Int =
    Hash.slot(keyCodec.write(key))

  def clusterKeyslot(key: Key): Result[Int] =
    execute(new ClusterKeyslot(key))

  private final class ClusterKeyslot(key: Key) extends RedisIntCommand with NodeCommand {
    val encoded = encoder("CLUSTER", "KEYSLOT").key(key).result
  }
}

trait NodeClusterApi extends KeyedClusterApi {
  def clusterAddslots(slot: Int, slots: Int*): Result[Unit] =
    execute(new ClusterAddslots(slot +:: slots))
  def clusterAddslots(slots: Iterable[Int]): Result[Unit] =
    execute(new ClusterAddslots(slots))
  def clusterCountFailureReports(nodeId: NodeId): Result[Long] =
    execute(new ClusterCountFailureReports(nodeId))
  def clusterCountkeysinslot(slot: Int): Result[Long] =
    execute(new ClusterCountkeysinslot(slot))
  def clusterDelslots(slot: Int, slots: Int*): Result[Unit] =
    execute(new ClusterDelslots(slot +:: slots))
  def clusterDelslots(slots: Iterable[Int]): Result[Unit] =
    execute(new ClusterDelslots(slots))
  def clusterFailover: Result[Unit] = clusterFailover()
  def clusterFailover(option: OptArg[FailoverOption] = OptArg.Empty): Result[Unit] =
    execute(new ClusterFailover(option.toOpt))
  def clusterForget(nodeId: NodeId): Result[Unit] =
    execute(new ClusterForget(nodeId))
  def clusterGetkeysinslot(slot: Int, count: Int): Result[Seq[Key]] =
    execute(new ClusterGetkeysinslot(slot, count))
  def clusterInfo: Result[ClusterStateInfo] =
    execute(ClusterInfo)
  def clusterMeet(address: NodeAddress): Result[Unit] =
    execute(new ClusterMeet(address))
  def clusterNodes: Result[Seq[NodeInfo]] =
    execute(ClusterNodes)
  def clusterReplicate(nodeId: NodeId): Result[Unit] =
    execute(new ClusterReplicate(nodeId))
  def clusterReset: Result[Unit] = clusterReset()
  def clusterReset(hard: Boolean = false): Result[Unit] =
    execute(new ClusterReset(hard))
  def clusterSaveconfig: Result[Unit] =
    execute(ClusterSaveconfig)
  def clusterSetConfigEpoch(configEpoch: Long): Result[Unit] =
    execute(new ClusterSetConfigEpoch(configEpoch))
  def clusterSetslot(slot: Int, subcommand: SetslotCmd): Result[Unit] =
    execute(new ClusterSetslot(slot, subcommand))
  def clusterSlaves(nodeId: NodeId): Result[Seq[NodeInfo]] =
    execute(new ClusterSlaves(nodeId))
  def clusterSlots: Result[Seq[SlotRangeMapping[NodeAddress]]] =
    execute(new ClusterSlots(SlotsNodeFormat.OnlyAddress))
  def clusterSlotsWithNodeIds: Result[Seq[SlotRangeMapping[(NodeAddress, NodeId)]]] =
    execute(new ClusterSlots(SlotsNodeFormat.AddressAndNodeId))

  private final class ClusterAddslots(slots: Iterable[Int]) extends RedisUnitCommand with NodeCommand {
    requireNonEmpty(slots, "slots")
    val encoded = encoder("CLUSTER", "ADDSLOTS").add(slots).result
  }

  private final class ClusterCountFailureReports(nodeId: NodeId) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("CLUSTER", "COUNT-FAILURE-REPORTS").add(nodeId.raw).result
  }

  private final class ClusterCountkeysinslot(slot: Int) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("CLUSTER", "COUNTKEYSINSLOT").add(slot).result
  }

  private final class ClusterDelslots(slots: Iterable[Int]) extends RedisUnitCommand with NodeCommand {
    requireNonEmpty(slots, "slots")
    val encoded = encoder("CLUSTER", "DELSLOTS").add(slots).result
  }

  private final class ClusterFailover(option: Opt[FailoverOption]) extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("CLUSTER", "FAILOVER").add(option).result
  }

  private final class ClusterForget(nodeId: NodeId) extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("CLUSTER", "FORGET").add(nodeId.raw).result
  }

  private final class ClusterGetkeysinslot(slot: Int, count: Int) extends RedisDataSeqCommand[Key] with NodeCommand {
    val encoded = encoder("CLUSTER", "GETKEYSINSLOT").add(slot).add(count).result
  }

  private object ClusterInfo
    extends AbstractRedisCommand[ClusterStateInfo](bulk(bs => ClusterStateInfo(bs.utf8String))) with NodeCommand {
    val encoded = encoder("CLUSTER", "INFO").result
  }

  private final class ClusterMeet(address: NodeAddress) extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("CLUSTER", "MEET").add(address.ip).add(address.port).result
  }

  private object ClusterNodes extends AbstractRedisCommand[Seq[NodeInfo]](bulkNodeInfos) with NodeCommand {
    val encoded = encoder("CLUSTER", "NODES").result
  }

  private final class ClusterReplicate(nodeId: NodeId) extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("CLUSTER", "REPLICATE").add(nodeId.raw).result
  }

  private final class ClusterReset(hard: Boolean) extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("CLUSTER", "RESET").addFlag("HARD", hard).result
  }

  private object ClusterSaveconfig extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("CLUSTER", "SAVECONFIG").result
  }

  private final class ClusterSetConfigEpoch(configEpoch: Long) extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("CLUSTER", "SET-CONFIG-EPOCH").add(configEpoch).result
  }

  private final class ClusterSetslot(slot: Int, subcommand: SetslotCmd) extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("CLUSTER", "SETSLOT").add(slot).add(subcommand).result
  }

  private final class ClusterSlaves(nodeId: NodeId) extends AbstractRedisCommand[Seq[NodeInfo]](bulkNodeInfos) with NodeCommand {
    val encoded = encoder("CLUSTER", "SLAVES").add(nodeId.raw).result
  }

  private final class ClusterSlots[N](nodeFormat: SlotsNodeFormat[N])
    extends RedisSeqCommand[SlotRangeMapping[N]](multiBulkSlotRangeMapping(nodeFormat)) with NodeCommand {
    val encoded = encoder("CLUSTER", "SLOTS").result
  }
}

trait ConnectionClusterApi extends NodeClusterApi {
  def readonly: Result[Unit] =
    execute(Readonly)
  def readwrite: Result[Unit] =
    execute(Readwrite)

  private object Readonly extends RedisUnitCommand with ConnectionCommand {
    val encoded = encoder("READONLY").result
  }

  private object Readwrite extends RedisUnitCommand with ConnectionCommand {
    val encoded = encoder("READWRITE").result
  }
}

case object Asking extends UnsafeCommand {
  val encoded = encoder("ASKING").result
}

trait SlotsNodeFormat[N] {
  def parseNode: PartialFunction[ArrayMsg[RedisMsg], N]
}
object SlotsNodeFormat {
  object OnlyAddress extends SlotsNodeFormat[NodeAddress] {
    def parseNode = {
      case ArrayMsg(IndexedSeq(BulkStringMsg(ip), IntegerMsg(port), _*)) =>
        NodeAddress(ip.utf8String, port.toInt)
    }
  }
  object AddressAndNodeId extends SlotsNodeFormat[(NodeAddress, NodeId)] {
    def parseNode = {
      case ArrayMsg(IndexedSeq(BulkStringMsg(ip), IntegerMsg(port), BulkStringMsg(nodeId), _*)) =>
        (NodeAddress(ip.utf8String, port.toInt), NodeId(nodeId.utf8String))
    }
  }
}

case class NodeId(raw: String) extends AnyVal

sealed abstract class FailoverOption(val name: String) extends NamedEnum
object FailoverOption extends NamedEnumCompanion[FailoverOption] {
  case object Force extends FailoverOption("FORCE")
  case object Takeover extends FailoverOption("TAKEOVER")

  val values: List[FailoverOption] = caseObjects
}

sealed trait SetslotCmd
object SetslotCmd {
  case class Migrating(destinationNodeId: NodeId) extends SetslotCmd
  case class Importing(sourceNodeId: NodeId) extends SetslotCmd
  case object Stable extends SetslotCmd
  case class Node(nodeId: NodeId) extends SetslotCmd

  implicit val SubcommandCommandArg: CommandArg[SetslotCmd] =
    CommandArg((encoder, arg) => arg match {
      case Migrating(NodeId(nodeId)) => encoder.add("MIGRATING").add(nodeId)
      case Importing(NodeId(nodeId)) => encoder.add("IMPORTING").add(nodeId)
      case Stable => encoder.add("STABLE")
      case Node(NodeId(nodeId)) => encoder.add("NODE").add(nodeId)
    })
}

case class ClusterStateInfo(info: String) extends ParsedInfo(info, "\r\n", ":") {
  val stateOk = attrMap("cluster_state") == "ok"
  val slotsAssigned = attrMap("cluster_slots_assigned").toInt
  val slotsOk = attrMap("cluster_slots_ok").toInt
  val slotsPfail = attrMap("cluster_slots_pfail").toInt
  val slotsFail = attrMap("cluster_slots_fail").toInt
  val knownNodes = attrMap("cluster_known_nodes").toInt
  val size = attrMap("cluster_size").toInt
  val currentEpoch = attrMap("cluster_current_epoch").toLong
  val myEpoch = attrMap("cluster_my_epoch").toLong
  val statsMessagesSent = attrMap("cluster_stats_messages_sent").toLong
  val statsMessagesReceived = attrMap("cluster_stats_messages_received").toLong
}

case class NodeInfo(infoLine: String) {
  private val splitLine: Array[String] = infoLine.split(' ')

  val id = NodeId(splitLine(0))
  val address = NodeAddress.parse(splitLine(1))
  val flags = NodeFlags(splitLine(2))
  val master = Opt(splitLine(3)).filter(_ != "-").map(NodeId)
  val pingSent = splitLine(4).toLong
  val pongRecv = splitLine(5).toLong
  val configEpoch = splitLine(6).toLong
  val connected = splitLine(7) == "connected"
  val (slots: Seq[SlotRange], importingSlots: Seq[(Int, NodeId)], migratingSlots: Seq[(Int, NodeId)]) = {

    val slots = new ArrayBuffer[SlotRange]
    val importingSlots = new ArrayBuffer[(Int, NodeId)]
    val migratingSlots = new ArrayBuffer[(Int, NodeId)]

    splitLine.iterator.drop(8).foreach { str =>
      (str.indexOf("-<-"), str.indexOf("->-"), str.indexOf('-')) match {
        case (-1, -1, -1) =>
          val slot = str.toInt
          slots += SlotRange(slot, slot)
        case (-1, -1, idx) =>
          slots += SlotRange(str.take(idx).toInt, str.drop(idx + 1).toInt)
        case (idx, -1, _) =>
          importingSlots += ((str.take(idx).toInt, NodeId(str.drop(idx + 1))))
        case (-1, idx, _) =>
          migratingSlots += ((str.take(idx).toInt, NodeId(str.drop(idx + 1))))
        case _ =>
      }
    }
    (slots, importingSlots, migratingSlots)
  }

  override def toString = infoLine
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

case class SlotRangeMapping[N](range: SlotRange, master: N, slaves: Seq[N]) {
  override def toString = s"slots: $range, master: $master, slaves: ${slaves.mkString(",")}"
}
case class SlotRange(start: Int, end: Int) {
  def toRange: Range = start to end
  def contains(slot: Int): Boolean = slot >= start && slot <= end
  override def toString = if (start == end) start.toString else s"$start-$end"
}
