package com.avsystem.commons
package redis.commands

import com.avsystem.commons.misc.{Opt => _, OptArg => _, _}
import com.avsystem.commons.redis.CommandEncoder.CommandArg
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.commands.ReplyDecoders._
import com.avsystem.commons.redis.protocol.SimpleStringMsg

import scala.collection.mutable

trait KeyedClusterApi extends ApiSubset {
  def keySlot(key: Key): Int =
    Hash.slot(keyCodec.write(key))

  /** Executes [[http://redis.io/commands/cluster-keyslot CLUSTER KEYSLOT]] */
  def clusterKeyslot(key: Key): Result[Int] =
    execute(new ClusterKeyslot(key))

  private final class ClusterKeyslot(key: Key) extends RedisIntCommand with NodeCommand {
    val encoded: Encoded = encoder("CLUSTER", "KEYSLOT").key(key).result
  }
}

trait NodeClusterApi extends KeyedClusterApi {
  /** Executes [[http://redis.io/commands/cluster-addslots CLUSTER ADDSLOTS]] */
  def clusterAddslots(slot: Int, slots: Int*): Result[Unit] =
    execute(new ClusterAddslots(slot +:: slots))

  /** Executes [[http://redis.io/commands/cluster-addslots CLUSTER ADDSLOTS]]
    * or does nothing when `slots` is empty. */
  def clusterAddslots(slots: Iterable[Int]): Result[Unit] =
    execute(new ClusterAddslots(slots))

  /** Executes [[http://redis.io/commands/cluster-bumpepoch CLUSTER BUMPEPOCH]] */
  def clusterBumpepoch: Result[BumpepochResult] =
    execute(ClusterBumpepoch)

  /** Executes [[http://redis.io/commands/cluster-count-failure-reports CLUSTER COUNT-FAILURE-REPORTS]] */
  def clusterCountFailureReports(nodeId: NodeId): Result[Long] =
    execute(new ClusterCountFailureReports(nodeId))

  /** Executes [[http://redis.io/commands/cluster-countkeysinslot CLUSTER COUNTKEYSINSLOT]] */
  def clusterCountkeysinslot(slot: Int): Result[Long] =
    execute(new ClusterCountkeysinslot(slot))

  /** Executes [[http://redis.io/commands/cluster-delslots CLUSTER DELSLOTS]] */
  def clusterDelslots(slot: Int, slots: Int*): Result[Unit] =
    execute(new ClusterDelslots(slot +:: slots))

  /** Executes [[http://redis.io/commands/cluster-delslots CLUSTER DELSLOTS]]
    * or does nothing when `slots` is empty */
  def clusterDelslots(slots: Iterable[Int]): Result[Unit] =
    execute(new ClusterDelslots(slots))

  /** Executes [[http://redis.io/commands/cluster-failover CLUSTER FAILOVER]] */
  def clusterFailover: Result[Unit] = clusterFailover()

  /** Executes [[http://redis.io/commands/cluster-failover CLUSTER FAILOVER]] */
  def clusterFailover(option: OptArg[FailoverOption] = OptArg.Empty): Result[Unit] =
    execute(new ClusterFailover(option.toOpt))

  /** Executes [[http://redis.io/commands/cluster-flushslots CLUSTER FLUSHSLOTS]] */
  def clusterFlushslots: Result[Unit] =
    execute(ClusterFlushslots)

  /** Executes [[http://redis.io/commands/cluster-forget CLUSTER FORGET]] */
  def clusterForget(nodeId: NodeId): Result[Unit] =
    execute(new ClusterForget(nodeId))

  /** Executes [[http://redis.io/commands/cluster-getkeysinslot CLUSTER GETKEYSINSLOT]] */
  def clusterGetkeysinslot(slot: Int, count: Int): Result[Seq[Key]] =
    execute(new ClusterGetkeysinslot(slot, count))

  /** Executes [[http://redis.io/commands/cluster-info CLUSTER INFO]] */
  def clusterInfo: Result[ClusterStateInfo] =
    execute(ClusterInfo)

  /** Executes [[http://redis.io/commands/cluster-meet CLUSTER MEET]] */
  def clusterMeet(address: NodeAddress): Result[Unit] =
    execute(new ClusterMeet(address))

  /** Executes [[http://redis.io/commands/cluster-myid CLUSTER MYID]] */
  def clusterMyid: Result[NodeId] =
    execute(ClusterMyid)

  /** Executes [[http://redis.io/commands/cluster-nodes CLUSTER NODES]] */
  def clusterNodes: Result[Seq[NodeInfo]] =
    execute(ClusterNodes)

  /** Executes [[http://redis.io/commands/cluster-replicas CLUSTER REPLICAS]] */
  def clusterReplicas(nodeId: NodeId): Result[Seq[NodeInfo]] =
    execute(new ClusterReplicas(nodeId))

  /** Executes [[http://redis.io/commands/cluster-replicate CLUSTER REPLICATE]] */
  def clusterReplicate(nodeId: NodeId): Result[Unit] =
    execute(new ClusterReplicate(nodeId))

  /** Executes [[http://redis.io/commands/cluster-reset CLUSTER RESET]] */
  def clusterReset: Result[Unit] = clusterReset()

  /** Executes [[http://redis.io/commands/cluster-reset CLUSTER RESET]] */
  def clusterReset(hard: Boolean = false): Result[Unit] =
    execute(new ClusterReset(hard))

  /** Executes [[http://redis.io/commands/cluster-saveconfig CLUSTER SAVECONFIG]] */
  def clusterSaveconfig: Result[Unit] =
    execute(ClusterSaveconfig)

  /** Executes [[http://redis.io/commands/cluster-set-config-epoch CLUSTER SET-CONFIG-EPOCH]] */
  def clusterSetConfigEpoch(configEpoch: Long): Result[Unit] =
    execute(new ClusterSetConfigEpoch(configEpoch))

  /** Executes [[http://redis.io/commands/cluster-setslot CLUSTER SETSLOT]] */
  def clusterSetslot(slot: Int, subcommand: SetslotCmd): Result[Unit] =
    execute(new ClusterSetslot(slot, subcommand))

  /** Executes [[http://redis.io/commands/cluster-slaves CLUSTER SLAVES]] */
  def clusterSlaves(nodeId: NodeId): Result[Seq[NodeInfo]] =
    execute(new ClusterSlaves(nodeId))

  /** Executes [[http://redis.io/commands/cluster-slots CLUSTER SLOTS]] */
  def clusterSlots: Result[Seq[SlotRangeMapping]] =
    execute(ClusterSlots)

  private final class ClusterAddslots(slots: Iterable[Int]) extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("CLUSTER", "ADDSLOTS").add(slots).result
    override def immediateResult: Opt[Unit] = whenEmpty(slots, ())
  }

  private object ClusterBumpepoch extends AbstractRedisCommand[BumpepochResult](simpleBumpepochResult) with NodeCommand {
    val encoded: Encoded = encoder("CLUSTER", "BUMPEPOCH").result
  }

  private final class ClusterCountFailureReports(nodeId: NodeId) extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("CLUSTER", "COUNT-FAILURE-REPORTS").add(nodeId.raw).result
  }

  private final class ClusterCountkeysinslot(slot: Int) extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("CLUSTER", "COUNTKEYSINSLOT").add(slot).result
  }

  private final class ClusterDelslots(slots: Iterable[Int]) extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("CLUSTER", "DELSLOTS").add(slots).result
    override def immediateResult: Opt[Unit] = whenEmpty(slots, ())
  }

  private final class ClusterFailover(option: Opt[FailoverOption]) extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("CLUSTER", "FAILOVER").optAdd(option).result
  }

  private object ClusterFlushslots extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("CLUSTER", "FLUSHSLOTS").result
  }

  private final class ClusterForget(nodeId: NodeId) extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("CLUSTER", "FORGET").add(nodeId.raw).result
  }

  private final class ClusterGetkeysinslot(slot: Int, count: Int) extends RedisDataSeqCommand[Key] with NodeCommand {
    val encoded: Encoded = encoder("CLUSTER", "GETKEYSINSLOT").add(slot).add(count).result
  }

  private object ClusterInfo
    extends AbstractRedisCommand[ClusterStateInfo](bulk(bs => ClusterStateInfo(bs.utf8String))) with NodeCommand {
    val encoded: Encoded = encoder("CLUSTER", "INFO").result
  }

  private final class ClusterMeet(address: NodeAddress) extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("CLUSTER", "MEET").add(address.ip).add(address.port).result
  }

  private final object ClusterMyid extends AbstractRedisCommand[NodeId](bulkAsNodeId) with NodeCommand {
    val encoded: Encoded = encoder("CLUSTER", "MYID").result
  }

  private object ClusterNodes extends AbstractRedisCommand[Seq[NodeInfo]](bulkAsNodeInfos) with NodeCommand {
    val encoded: Encoded = encoder("CLUSTER", "NODES").result
  }

  private final class ClusterReplicas(nodeId: NodeId) extends AbstractRedisCommand[Seq[NodeInfo]](multiBulkAsNodeInfos) with NodeCommand {
    val encoded: Encoded = encoder("CLUSTER", "REPLICAS").add(nodeId.raw).result
  }

  private final class ClusterReplicate(nodeId: NodeId) extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("CLUSTER", "REPLICATE").add(nodeId.raw).result
  }

  private final class ClusterReset(hard: Boolean) extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("CLUSTER", "RESET").addFlag("HARD", hard).result
  }

  private object ClusterSaveconfig extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("CLUSTER", "SAVECONFIG").result
  }

  private final class ClusterSetConfigEpoch(configEpoch: Long) extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("CLUSTER", "SET-CONFIG-EPOCH").add(configEpoch).result
  }

  private final class ClusterSetslot(slot: Int, subcommand: SetslotCmd) extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("CLUSTER", "SETSLOT").add(slot).add(subcommand).result
  }

  private final class ClusterSlaves(nodeId: NodeId) extends AbstractRedisCommand[Seq[NodeInfo]](multiBulkAsNodeInfos) with NodeCommand {
    val encoded: Encoded = encoder("CLUSTER", "SLAVES").add(nodeId.raw).result
  }

  private object ClusterSlots
    extends RedisSeqCommand[SlotRangeMapping](multiBulkAsSlotRangeMapping) with NodeCommand {
    val encoded: Encoded = encoder("CLUSTER", "SLOTS").result
  }
}

trait ConnectionClusterApi extends NodeClusterApi {
  /** Executes [[http://redis.io/commands/readonly READONLY]] */
  def readonly: Result[Unit] =
    execute(Readonly)
  /** Executes [[http://redis.io/commands/readwrite READWRITE]] */
  def readwrite: Result[Unit] =
    execute(Readwrite)

  private object Readonly extends RedisUnitCommand with ConnectionCommand {
    val encoded: Encoded = encoder("READONLY").result
  }

  private object Readwrite extends RedisUnitCommand with ConnectionCommand {
    val encoded: Encoded = encoder("READWRITE").result
  }
}

case object Asking extends UnsafeCommand {
  val encoded: Encoded = encoder("ASKING").result
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
  val stateOk: Boolean = attrMap("cluster_state") == "ok"
  val slotsAssigned: Int = attrMap("cluster_slots_assigned").toInt
  val slotsOk: Int = attrMap("cluster_slots_ok").toInt
  val slotsPfail: Int = attrMap("cluster_slots_pfail").toInt
  val slotsFail: Int = attrMap("cluster_slots_fail").toInt
  val knownNodes: Int = attrMap("cluster_known_nodes").toInt
  val size: Int = attrMap("cluster_size").toInt
  val currentEpoch: Long = attrMap("cluster_current_epoch").toLong
  val myEpoch: Long = attrMap("cluster_my_epoch").toLong
  val statsMessagesSent: Long = attrMap("cluster_stats_messages_sent").toLong
  val statsMessagesReceived: Long = attrMap("cluster_stats_messages_received").toLong
}

case class NodeInfo(infoLine: String) {
  private val splitLine: Array[String] = infoLine.split(' ')
  private val splitAddr: Array[String] = splitLine(1).split('@')

  val id: NodeId = NodeId(splitLine(0))
  val address: NodeAddress = NodeAddress.parse(splitAddr(0))
  val clusterPort: Opt[String] = splitAddr.opt.filter(_.length > 1).map(_.apply(1))
  val flags: NodeFlags = NodeFlags(splitLine(2))
  val master: Opt[NodeId] = Opt(splitLine(3)).filter(_ != "-").map(NodeId.apply)
  val pingSent: Long = splitLine(4).toLong
  val pongRecv: Long = splitLine(5).toLong
  val configEpoch: Long = splitLine(6).toLong
  val connected: Boolean = splitLine(7) == "connected"

  val (slots: Seq[SlotRange], importingSlots: Seq[(Int, NodeId)], migratingSlots: Seq[(Int, NodeId)]) = {
    val slots = mutable.ArrayBuilder.make[SlotRange]
    val importingSlots = mutable.ArrayBuilder.make[(Int, NodeId)]
    val migratingSlots = mutable.ArrayBuilder.make[(Int, NodeId)]

    splitLine.iterator.drop(8).foreach { str =>
      (str.indexOf("-<-"), str.indexOf("->-"), str.indexOf('-')) match {
        case (-1, -1, -1) =>
          val slot = str.toInt
          slots += SlotRange(slot, slot)
        case (-1, -1, idx) =>
          slots += SlotRange(str.take(idx).toInt, str.drop(idx + 1).toInt)
        case (idx, -1, _) =>
          importingSlots += ((str.substring(1, idx).toInt, NodeId(str.substring(idx + 1, str.length - 1))))
        case (-1, idx, _) =>
          migratingSlots += ((str.substring(1, idx).toInt, NodeId(str.substring(idx + 1, str.length - 1))))
        case _ =>
      }
    }

    def res[T](b: mutable.ArrayBuilder[T]): IndexedSeq[T] =
      IArraySeq.unsafeWrapArray(b.result())

    (res(slots), res(importingSlots), res(migratingSlots))
  }

  override def toString: String = infoLine
}

class NodeFlags(val raw: Int) extends AnyVal {

  import NodeFlags._

  def |(other: NodeFlags): NodeFlags = new NodeFlags(raw | other.raw)
  def &(other: NodeFlags): NodeFlags = new NodeFlags(raw & other.raw)
  def ^(other: NodeFlags): NodeFlags = new NodeFlags(raw ^ other.raw)
  def unary_~ : NodeFlags = new NodeFlags(~raw)

  def myself: Boolean = (this & Myself) != Noflags
  def master: Boolean = (this & Master) != Noflags
  def slave: Boolean = (this & Slave) != Noflags
  def pfail: Boolean = (this & Pfail) != Noflags
  def fail: Boolean = (this & Fail) != Noflags
  def handshake: Boolean = (this & Handshake) != Noflags
  def noaddr: Boolean = (this & Noaddr) != Noflags

  override def toString: String =
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
    val flagSet = str.split(',').to(mutable.HashSet)
    reprValuePairs.foldLeft(Noflags) {
      case (res, (s, flags)) => if (flagSet(s)) res | flags else res
    }
  }
}

case class SlotRangeMapping(
  range: SlotRange, master: NodeAddress, masterId: Opt[NodeId], slaves: Seq[(NodeAddress, Opt[NodeId])]
) {
  private def nodeRepr(addr: NodeAddress, idOpt: Opt[NodeId]): String =
    addr.toString + idOpt.fold("")(id => s" (${id.raw})")
  override def toString: String =
    s"slots: $range, master: ${nodeRepr(master, masterId)}, slaves: ${slaves.map((nodeRepr _).tupled).mkString(",")}"
}
case class SlotRange(start: Int, end: Int) {
  def toRange: Range = start to end
  def contains(slot: Int): Boolean = slot >= start && slot <= end
  override def toString: String = if (start == end) start.toString else s"$start-$end"
}
object SlotRange {
  final val LastSlot = Hash.TotalSlots - 1
  final val Full = SlotRange(0, LastSlot)
}

final class BumpepochResult(implicit enumCtx: EnumCtx) extends AbstractValueEnum {
  val encoded: SimpleStringMsg = SimpleStringMsg(name.toUpperCase)
}
object BumpepochResult extends AbstractValueEnumCompanion[BumpepochResult] {
  final val Bumped, Still: Value = new BumpepochResult
}
