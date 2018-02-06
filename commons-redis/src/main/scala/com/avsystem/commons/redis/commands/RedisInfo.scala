package com.avsystem.commons
package redis.commands

import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion}
import com.avsystem.commons.redis.NodeAddress

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

abstract class RedisInfoSection[T >: FullRedisInfo <: RedisInfo](val name: String)
trait RedisInfo {
  val infoStr: String

  protected def indexedPrefixes: List[String] = Nil

  lazy val rawValues: BMap[String, String] =
    mutable.LinkedHashMap() ++= Source.fromString(infoStr).getLines()
      .map(_.trim).filterNot(l => l.isEmpty || l.startsWith("#"))
      .map(l => l.split(":", 2)).map({ case Array(key, value) => (key, value) })

  protected lazy val keysByPrefix: BMap[String, Seq[String]] = {
    val result = new MHashMap[String, ListBuffer[String]]
    rawValues.keys.foreach { key =>
      indexedPrefixes.filter(key.startsWith).foreach { prefix =>
        result.getOrElseUpdate(prefix, new ListBuffer) += key
      }
    }
    result
  }

  def get(name: String): Opt[String] =
    rawValues.get(name).toOpt

  override def toString: String = infoStr
}

abstract class DefaultRedisInfo extends RedisInfo
  with ServerInfo
  with ClientsInfo
  with MemoryInfo
  with PersistenceInfo
  with StatsInfo
  with ReplicationInfo
  with CpuInfo
  with ClusterInfo
  with KeyspaceInfo
object DefaultRedisInfo extends RedisInfoSection[DefaultRedisInfo]("default")

case class FullRedisInfo(infoStr: String) extends DefaultRedisInfo
  with CommandstatsInfo
object FullRedisInfo extends RedisInfoSection[FullRedisInfo]("all")

trait ServerInfo extends RedisInfo {
  def redisVersion: Opt[String] = get("redis_version")
  def redisGitSha1: Opt[String] = get("redis_git_sha1")
  def redisGitDirty: Opt[Boolean] = get("redis_git_dirty").map(_ == "1")
  def redisBuildId: Opt[String] = get("redis_build_id")
  def redisMode: Opt[RedisMode] = get("redis_mode").map(RedisMode.byName)
  def os: Opt[String] = get("os")
  def archBits: Opt[Int] = get("arch_bits").map(_.toInt)
  def multiplexingApi: Opt[String] = get("multiplexing_api")
  def gccVersion: Opt[String] = get("gcc_version")
  def processId: Opt[Int] = get("process_id").map(_.toInt)
  def runId: Opt[String] = get("run_id")
  def tcpPort: Opt[Int] = get("tcp_port").map(_.toInt)
  def uptimeInSeconds: Opt[Long] = get("uptime_in_seconds").map(_.toLong)
  def uptimeInDays: Opt[Long] = get("uptime_in_days").map(_.toLong)
  def hz: Opt[Int] = get("hz").map(_.toInt)
  def lruClock: Opt[Long] = get("lru_clock").map(_.toLong)
  def executable: Opt[String] = get("executable")
  def configFile: Opt[String] = get("config_file")
}
object ServerInfo extends RedisInfoSection[ServerInfo]("server")

sealed abstract class RedisMode(val name: String) extends NamedEnum
object RedisMode extends NamedEnumCompanion[RedisMode] {
  case object Cluster extends RedisMode("cluster")
  case object Sentinel extends RedisMode("sentinel")
  case object Standalone extends RedisMode("standalone")
  val values: List[RedisMode] = caseObjects
}
trait ClientsInfo extends RedisInfo {
  def connectedClients: Opt[Long] = get("connected_clients").map(_.toLong)
  def clientLongestOutputList: Opt[Long] = get("client_longest_output_list").map(_.toLong)
  def clientBiggestInputBuf: Opt[Long] = get("client_biggest_input_buf").map(_.toLong)
  def blockedClients: Opt[Int] = get("blocked_clients").map(_.toInt)
}
object ClientsInfo extends RedisInfoSection[ClientsInfo]("clients")

trait MemoryInfo extends RedisInfo {
  def usedMemory: Opt[Long] = get("used_memory").map(_.toLong)
  def usedMemoryHuman: Opt[String] = get("used_memory_human")
  def usedMemoryRss: Opt[Long] = get("used_memory_rss").map(_.toLong)
  def usedMemoryRssHuman: Opt[String] = get("used_memory_rss_human")
  def usedMemoryPeak: Opt[Long] = get("used_memory_peak").map(_.toLong)
  def usedMemoryPeakHuman: Opt[String] = get("used_memory_peak_human")
  def totalSystemMemory: Opt[Long] = get("total_system_memory").map(_.toLong)
  def totalSystemMemoryHuman: Opt[String] = get("total_system_memory_human")
  def usedMemoryLua: Opt[Long] = get("used_memory_lua").map(_.toLong)
  def usedMemoryLuaHuman: Opt[String] = get("used_memory_lua_human")
  def maxmemory: Opt[Long] = get("maxmemory").map(_.toLong)
  def maxmemoryHuman: Opt[String] = get("maxmemory_human")
  def maxmemoryPolicy: Opt[MaxmemoryPolicy] = get("maxmemory_policy").map(MaxmemoryPolicy.byName)
  def memFragmentationRatio: Opt[Double] = get("mem_fragmentation_ratio").map(_.toDouble)
  def memAllocator: Opt[String] = get("mem_allocator")
}
object MemoryInfo extends RedisInfoSection[MemoryInfo]("memory")

sealed abstract class MaxmemoryPolicy(val name: String) extends NamedEnum
object MaxmemoryPolicy extends NamedEnumCompanion[MaxmemoryPolicy] {
  case object VolatileLru extends MaxmemoryPolicy("volatile-lru")
  case object AllkeysLru extends MaxmemoryPolicy("allkeys-lru")
  case object VolatileRandom extends MaxmemoryPolicy("volatile-random")
  case object AllkeysRandom extends MaxmemoryPolicy("allkeys-random")
  case object VolatileTtl extends MaxmemoryPolicy("volatile-ttl")
  case object Noeviction extends MaxmemoryPolicy("noeviction")
  val values: List[MaxmemoryPolicy] = caseObjects
}

trait PersistenceInfo extends RedisInfo {
  def loading: Opt[Boolean] = get("loading").map(_ == "1")
  def rdbChangesSinceLastSave: Opt[Long] = get("rdb_changes_since_last_save").map(_.toLong)
  def rdbBgsaveInProgress: Opt[Boolean] = get("rdb_bgsave_in_progress").map(_ == "1")
  def rdbLastSaveTime: Opt[Long] = get("rdb_last_save_time").map(_.toLong)
  def rdbLastBgsaveStatusOk: Opt[Boolean] = get("rdb_last_bgsave_status").map(_ == "ok")
  def rdbLastBgsaveTimeSec: Opt[Long] = get("rdb_last_bgsave_time_sec").map(_.toLong)
  def rdbCurrentBgsaveTimeSec: Opt[Long] = get("rdb_current_bgsave_time_sec").map(_.toLong)
  def aofEnabled: Opt[Boolean] = get("aof_enabled").map(_ == "1")
  def aofRewriteInProgress: Opt[Boolean] = get("aof_rewrite_in_progress").map(_ == "1")
  def aofRewriteScheduled: Opt[Boolean] = get("aof_rewrite_scheduled").map(_ == "1")
  def aofLastRewriteTimeSec: Opt[Long] = get("aof_last_rewrite_time_sec").map(_.toLong)
  def aofCurrentRewriteTimeSec: Opt[Long] = get("aof_current_rewrite_time_sec").map(_.toLong)
  def aofLastBgrewriteStatusOk: Opt[Boolean] = get("aof_last_bgrewrite_status").map(_ == "ok")
  def aofLastWriteStatusOk: Opt[Boolean] = get("aof_last_write_status").map(_ == "ok")
  def aofCurrentSize: Opt[Long] = get("aof_current_size").map(_.toLong)
  def aofBaseSize: Opt[Long] = get("aof_base_size").map(_.toLong)
  def aofPendingRewrite: Opt[Boolean] = get("aof_pending_rewrite").map(_ == "1")
  def aofBufferLength: Opt[Long] = get("aof_buffer_length").map(_.toLong)
  def aofRewriteBufferLength: Opt[Long] = get("aof_rewrite_buffer_length").map(_.toLong)
  def aofPendingBioFsync: Opt[Long] = get("aof_pending_bio_fsync").map(_.toLong)
  def aofDelayedFsync: Opt[Long] = get("aof_delayed_fsync").map(_.toLong)
  def loadingStartTime: Opt[Long] = get("loading_start_time").map(_.toLong)
  def loadingTotalBytes: Opt[Long] = get("loading_total_bytes").map(_.toLong)
  def loadingLoadedBytes: Opt[Long] = get("loading_loaded_bytes").map(_.toLong)
  def loadingLoadedPerc: Opt[Double] = get("loading_loaded_perc").map(_.toDouble)
  def loadingEtaSeconds: Opt[Long] = get("loading_eta_seconds").map(_.toLong)
}
object PersistenceInfo extends RedisInfoSection[PersistenceInfo]("persistence")

trait StatsInfo extends RedisInfo {
  def totalConnectionsReceived: Opt[Long] = get("total_connections_received").map(_.toLong)
  def totalCommandsProcessed: Opt[Long] = get("total_commands_processed").map(_.toLong)
  def instantaneousOpsPerSec: Opt[Long] = get("instantaneous_ops_per_sec").map(_.toLong)
  def totalNetInputBytes: Opt[Long] = get("total_net_input_bytes").map(_.toLong)
  def totalNetOutputBytes: Opt[Long] = get("total_net_output_bytes").map(_.toLong)
  def instantaneousInputKbps: Opt[Double] = get("instantaneous_input_kbps").map(_.toDouble)
  def instantaneousOutputKbps: Opt[Double] = get("instantaneous_output_kbps").map(_.toDouble)
  def rejectedConnections: Opt[Long] = get("rejected_connections").map(_.toLong)
  def syncFull: Opt[Long] = get("sync_full").map(_.toLong)
  def syncPartialOk: Opt[Long] = get("sync_partial_ok").map(_.toLong)
  def syncPartialErr: Opt[Long] = get("sync_partial_err").map(_.toLong)
  def expiredKeys: Opt[Long] = get("expired_keys").map(_.toLong)
  def evictedKeys: Opt[Long] = get("evicted_keys").map(_.toLong)
  def keyspaceHits: Opt[Long] = get("keyspace_hits").map(_.toLong)
  def keyspaceMisses: Opt[Long] = get("keyspace_misses").map(_.toLong)
  def pubsubChannels: Opt[Long] = get("pubsub_channels").map(_.toLong)
  def pubsubPatterns: Opt[Long] = get("pubsub_patterns").map(_.toLong)
  def latestForkUsec: Opt[Long] = get("latest_fork_usec").map(_.toLong)
  def migrateCachedSockets: Opt[Long] = get("migrate_cached_sockets").map(_.toLong)
}
object StatsInfo extends RedisInfoSection[StatsInfo]("stats")

trait ReplicationInfo extends RedisInfo {
  def roleMaster: Opt[Boolean] = get("role").map(_ == "master")
  def masterAddr: Opt[NodeAddress] =
    for {
      masterHost <- get("master_host")
      masterPort <- get("master_port").map(_.toInt)
    } yield NodeAddress(masterHost, masterPort)
  def masterLinkStatusUp: Opt[Boolean] = get("master_link_status").map(_ == "up")
  def masterLastIoSecondsAgo: Opt[Int] = get("master_last_io_seconds_ago").map(_.toInt)
  def masterSyncInProgress: Opt[Boolean] = get("master_sync_in_progress").map(_ == "1")
  def slaveReplOffset: Opt[Long] = get("slave_repl_offset").map(_.toLong)
  def masterSyncLeftBytes: Opt[Long] = get("master_sync_left_bytes").map(_.toLong)
  def masterSyncLastIoSecondsAgo: Opt[Int] = get("master_sync_last_io_seconds_ago").map(_.toInt)
  def masterLinkDownSinceSeconds: Opt[Long] = get("master_link_down_since_seconds").map(_.toLong)
  def slavePriority: Opt[Int] = get("slave_priority").map(_.toInt)
  def slaveReadonly: Opt[Boolean] = get("slave_read_only").map(_ == "1")
  def connectedSlaves: Opt[Int] = get("connected_slaves").map(_.toInt)
  def minSlavesGoodSlaves: Opt[Int] = get("min_slaves_good_slaves").map(_.toInt)
  /**
    * @param slaveId ranges from 0 to [[connectedSlaves]]-1
    */
  def slaveInfo(slaveId: Int): Opt[SlaveInfo] = get(s"slave$slaveId").map(SlaveInfo)
  def masterReplOffset: Opt[Long] = get("master_repl_offset").map(_.toLong)
  def replBacklogActive: Opt[Boolean] = get("repl_backlog_active").map(_ == "1")
  def replBacklogSize: Opt[Long] = get("repl_backlog_size").map(_.toLong)
  def replBacklogFirstByteOffset: Opt[Long] = get("repl_backlog_first_byte_offset").map(_.toLong)
  def replBacklogHistlen: Opt[Long] = get("repl_backlog_histlen").map(_.toLong)
}
object ReplicationInfo extends RedisInfoSection[ReplicationInfo]("replication")

case class SlaveInfo(infoLine: String) extends ParsedInfo(infoLine, ",", "=") {
  def addr = NodeAddress(attrMap("ip"), attrMap("port").toInt)
  def state = SlaveState.byName(attrMap("state"))
  def offset: Long = attrMap("offset").toLong
  def lag: Long = attrMap("lag").toLong
}
sealed abstract class SlaveState(val name: String) extends NamedEnum
object SlaveState extends NamedEnumCompanion[SlaveState] {
  object WaitBgsave extends SlaveState("wait_bgsave")
  object SendBulk extends SlaveState("send_bulk")
  object Online extends SlaveState("online")
  val values: List[SlaveState] = caseObjects
}

trait CpuInfo extends RedisInfo {
  def usedCpuSys: Opt[Double] = get("used_cpu_sys").map(_.toDouble)
  def usedCpuUser: Opt[Double] = get("used_cpu_user").map(_.toDouble)
  def usedCpuSysChildren: Opt[Double] = get("used_cpu_sys_children").map(_.toDouble)
  def usedCpuUserChildren: Opt[Double] = get("used_cpu_user_children").map(_.toDouble)
}
object CpuInfo extends RedisInfoSection[CpuInfo]("cpu")

trait CommandstatsInfo extends RedisInfo {
  override protected def indexedPrefixes: List[String] = "cmdstat_" :: super.indexedPrefixes

  lazy val executedCommands: BSeq[String] = keysByPrefix.getOrElse("cmdstat_", Nil).map(_.stripPrefix("cmdstat_"))
  def commandStat(command: String): Opt[CommandStat] = get(s"cmdstat_$command").map(CommandStat)
}
object CommandstatsInfo extends RedisInfoSection[CommandstatsInfo]("commandstats")

case class CommandStat(infoLine: String) extends ParsedInfo(infoLine, ",", "=") {
  def calls: Long = attrMap("calls").toLong
  def usec: Long = attrMap("usec").toLong
  def usecPerCall: Double = attrMap("usec_per_call").toDouble
}

trait ClusterInfo extends RedisInfo {
  def clusterEnabled: Opt[Boolean] = get("cluster_enabled").map(_ == "1")
}
object ClusterInfo extends RedisInfoSection[ClusterInfo]("cluster")

trait KeyspaceInfo extends RedisInfo {
  override protected def indexedPrefixes: List[String] = "db" :: super.indexedPrefixes

  lazy val nonEmptyDbs: Seq[Int] = keysByPrefix.getOrElse("db", Nil).map(_.stripPrefix("db").toInt)
  def dbStat(dbId: Int): Opt[DbStat] = get(s"db$dbId").map(DbStat)
}
object KeyspaceInfo extends RedisInfoSection[KeyspaceInfo]("keyspace")

case class DbStat(infoLine: String) extends ParsedInfo(infoLine, ",", "=") {
  def keys: Long = attrMap("keys").toLong
  def expires: Long = attrMap("expires").toLong
  def avgTtl: Long = attrMap("avg_ttl").toLong
}
