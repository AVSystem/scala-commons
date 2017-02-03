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

  override def toString = infoStr
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
  def redisVersion = get("redis_version")
  def redisGitSha1 = get("redis_git_sha1")
  def redisGitDirty = get("redis_git_dirty").map(_ == "1")
  def redisBuildId = get("redis_build_id")
  def redisMode = get("redis_mode").map(RedisMode.byName)
  def os = get("os")
  def archBits = get("arch_bits").map(_.toInt)
  def multiplexingApi = get("multiplexing_api")
  def gccVersion = get("gcc_version")
  def processId = get("process_id").map(_.toInt)
  def runId = get("run_id")
  def tcpPort = get("tcp_port").map(_.toInt)
  def uptimeInSeconds = get("uptime_in_seconds").map(_.toLong)
  def uptimeInDays = get("uptime_in_days").map(_.toLong)
  def hz = get("hz").map(_.toInt)
  def lruClock = get("lru_clock").map(_.toLong)
  def executable = get("executable")
  def configFile = get("config_file")
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
  def connectedClients = get("connected_clients").map(_.toLong)
  def clientLongestOutputList = get("client_longest_output_list").map(_.toLong)
  def clientBiggestInputBuf = get("client_biggest_input_buf").map(_.toLong)
  def blockedClients = get("blocked_clients").map(_.toInt)
}
object ClientsInfo extends RedisInfoSection[ClientsInfo]("clients")

trait MemoryInfo extends RedisInfo {
  def usedMemory = get("used_memory").map(_.toLong)
  def usedMemoryHuman = get("used_memory_human")
  def usedMemoryRss = get("used_memory_rss").map(_.toLong)
  def usedMemoryRssHuman = get("used_memory_rss_human")
  def usedMemoryPeak = get("used_memory_peak").map(_.toLong)
  def usedMemoryPeakHuman = get("used_memory_peak_human")
  def totalSystemMemory = get("total_system_memory").map(_.toLong)
  def totalSystemMemoryHuman = get("total_system_memory_human")
  def usedMemoryLua = get("used_memory_lua").map(_.toLong)
  def usedMemoryLuaHuman = get("used_memory_lua_human")
  def maxmemory = get("maxmemory").map(_.toLong)
  def maxmemoryHuman = get("maxmemory_human")
  def maxmemoryPolicy = get("maxmemory_policy").map(MaxmemoryPolicy.byName)
  def memFragmentationRatio = get("mem_fragmentation_ratio").map(_.toDouble)
  def memAllocator = get("mem_allocator")
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
  def loading = get("loading").map(_ == "1")
  def rdbChangesSinceLastSave = get("rdb_changes_since_last_save").map(_.toLong)
  def rdbBgsaveInProgress = get("rdb_bgsave_in_progress").map(_ == "1")
  def rdbLastSaveTime = get("rdb_last_save_time").map(_.toLong)
  def rdbLastBgsaveStatusOk = get("rdb_last_bgsave_status").map(_ == "ok")
  def rdbLastBgsaveTimeSec = get("rdb_last_bgsave_time_sec").map(_.toLong)
  def rdbCurrentBgsaveTimeSec = get("rdb_current_bgsave_time_sec").map(_.toLong)
  def aofEnabled = get("aof_enabled").map(_ == "1")
  def aofRewriteInProgress = get("aof_rewrite_in_progress").map(_ == "1")
  def aofRewriteScheduled = get("aof_rewrite_scheduled").map(_ == "1")
  def aofLastRewriteTimeSec = get("aof_last_rewrite_time_sec").map(_.toLong)
  def aofCurrentRewriteTimeSec = get("aof_current_rewrite_time_sec").map(_.toLong)
  def aofLastBgrewriteStatusOk = get("aof_last_bgrewrite_status").map(_ == "ok")
  def aofLastWriteStatusOk = get("aof_last_write_status").map(_ == "ok")
  def aofCurrentSize = get("aof_current_size").map(_.toLong)
  def aofBaseSize = get("aof_base_size").map(_.toLong)
  def aofPendingRewrite = get("aof_pending_rewrite").map(_ == "1")
  def aofBufferLength = get("aof_buffer_length").map(_.toLong)
  def aofRewriteBufferLength = get("aof_rewrite_buffer_length").map(_.toLong)
  def aofPendingBioFsync = get("aof_pending_bio_fsync").map(_.toLong)
  def aofDelayedFsync = get("aof_delayed_fsync").map(_.toLong)
  def loadingStartTime = get("loading_start_time").map(_.toLong)
  def loadingTotalBytes = get("loading_total_bytes").map(_.toLong)
  def loadingLoadedBytes = get("loading_loaded_bytes").map(_.toLong)
  def loadingLoadedPerc = get("loading_loaded_perc").map(_.toDouble)
  def loadingEtaSeconds = get("loading_eta_seconds").map(_.toLong)
}
object PersistenceInfo extends RedisInfoSection[PersistenceInfo]("persistence")

trait StatsInfo extends RedisInfo {
  def totalConnectionsReceived = get("total_connections_received").map(_.toLong)
  def totalCommandsProcessed = get("total_commands_processed").map(_.toLong)
  def instantaneousOpsPerSec = get("instantaneous_ops_per_sec").map(_.toLong)
  def totalNetInputBytes = get("total_net_input_bytes").map(_.toLong)
  def totalNetOutputBytes = get("total_net_output_bytes").map(_.toLong)
  def instantaneousInputKbps = get("instantaneous_input_kbps").map(_.toDouble)
  def instantaneousOutputKbps = get("instantaneous_output_kbps").map(_.toDouble)
  def rejectedConnections = get("rejected_connections").map(_.toLong)
  def syncFull = get("sync_full").map(_.toLong)
  def syncPartialOk = get("sync_partial_ok").map(_.toLong)
  def syncPartialErr = get("sync_partial_err").map(_.toLong)
  def expiredKeys = get("expired_keys").map(_.toLong)
  def evictedKeys = get("evicted_keys").map(_.toLong)
  def keyspaceHits = get("keyspace_hits").map(_.toLong)
  def keyspaceMisses = get("keyspace_misses").map(_.toLong)
  def pubsubChannels = get("pubsub_channels").map(_.toLong)
  def pubsubPatterns = get("pubsub_patterns").map(_.toLong)
  def latestForkUsec = get("latest_fork_usec").map(_.toLong)
  def migrateCachedSockets = get("migrate_cached_sockets").map(_.toLong)
}
object StatsInfo extends RedisInfoSection[StatsInfo]("stats")

trait ReplicationInfo extends RedisInfo {
  def roleMaster = get("role").map(_ == "master")
  def masterAddr =
    for {
      masterHost <- get("master_host")
      masterPort <- get("master_port").map(_.toInt)
    } yield NodeAddress(masterHost, masterPort)
  def masterLinkStatusUp = get("master_link_status").map(_ == "up")
  def masterLastIoSecondsAgo = get("master_last_io_seconds_ago").map(_.toInt)
  def masterSyncInProgress = get("master_sync_in_progress").map(_ == "1")
  def slaveReplOffset = get("slave_repl_offset").map(_.toLong)
  def masterSyncLeftBytes = get("master_sync_left_bytes").map(_.toLong)
  def masterSyncLastIoSecondsAgo = get("master_sync_last_io_seconds_ago").map(_.toInt)
  def masterLinkDownSinceSeconds = get("master_link_down_since_seconds").map(_.toLong)
  def slavePriority = get("slave_priority").map(_.toInt)
  def slaveReadonly = get("slave_read_only").map(_ == "1")
  def connectedSlaves = get("connected_slaves").map(_.toInt)
  def minSlavesGoodSlaves = get("min_slaves_good_slaves").map(_.toInt)
  /**
    * @param slaveId ranges from 0 to [[connectedSlaves]]-1
    */
  def slaveInfo(slaveId: Int) = get(s"slave$slaveId").map(SlaveInfo)
  def masterReplOffset = get("master_repl_offset").map(_.toLong)
  def replBacklogActive = get("repl_backlog_active").map(_ == "1")
  def replBacklogSize = get("repl_backlog_size").map(_.toLong)
  def replBacklogFirstByteOffset = get("repl_backlog_first_byte_offset").map(_.toLong)
  def replBacklogHistlen = get("repl_backlog_histlen").map(_.toLong)
}
object ReplicationInfo extends RedisInfoSection[ReplicationInfo]("replication")

case class SlaveInfo(infoLine: String) extends ParsedInfo(infoLine, ",", "=") {
  def addr = NodeAddress(attrMap("ip"), attrMap("port").toInt)
  def state = SlaveState.byName(attrMap("state"))
  def offset = attrMap("offset").toLong
  def lag = attrMap("lag").toLong
}
sealed abstract class SlaveState(val name: String) extends NamedEnum
object SlaveState extends NamedEnumCompanion[SlaveState] {
  object WaitBgsave extends SlaveState("wait_bgsave")
  object SendBulk extends SlaveState("send_bulk")
  object Online extends SlaveState("online")
  val values: List[SlaveState] = caseObjects
}

trait CpuInfo extends RedisInfo {
  def usedCpuSys = get("used_cpu_sys").map(_.toDouble)
  def usedCpuUser = get("used_cpu_user").map(_.toDouble)
  def usedCpuSysChildren = get("used_cpu_sys_children").map(_.toDouble)
  def usedCpuUserChildren = get("used_cpu_user_children").map(_.toDouble)
}
object CpuInfo extends RedisInfoSection[CpuInfo]("cpu")

trait CommandstatsInfo extends RedisInfo {
  override protected def indexedPrefixes = "cmdstat_" :: super.indexedPrefixes

  lazy val executedCommands = keysByPrefix.getOrElse("cmdstat_", Nil).map(_.stripPrefix("cmdstat_"))
  def commandStat(command: String) = get(s"cmdstat_$command").map(CommandStat)
}
object CommandstatsInfo extends RedisInfoSection[CommandstatsInfo]("commandstats")

case class CommandStat(infoLine: String) extends ParsedInfo(infoLine, ",", "=") {
  def calls = attrMap("calls").toLong
  def usec = attrMap("usec").toLong
  def usecPerCall = attrMap("usec_per_call").toDouble
}

trait ClusterInfo extends RedisInfo {
  def clusterEnabled = get("cluster_enabled").map(_ == "1")
}
object ClusterInfo extends RedisInfoSection[ClusterInfo]("cluster")

trait KeyspaceInfo extends RedisInfo {
  override protected def indexedPrefixes = "db" :: super.indexedPrefixes

  lazy val nonEmptyDbs: Seq[Int] = keysByPrefix.getOrElse("db", Nil).map(_.stripPrefix("db").toInt)
  def dbStat(dbId: Int) = get(s"db$dbId").map(DbStat)
}
object KeyspaceInfo extends RedisInfoSection[KeyspaceInfo]("keyspace")

case class DbStat(infoLine: String) extends ParsedInfo(infoLine, ",", "=") {
  def keys = attrMap("keys").toLong
  def expires = attrMap("expires").toLong
  def avgTtl = attrMap("avg_ttl").toLong
}
