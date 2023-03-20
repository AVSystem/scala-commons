package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis.commands.ReplyDecoders._
import com.avsystem.commons.redis._

trait SentinelApi extends ApiSubset {
  def sentinelMasters: Result[Seq[BMap[String, String]]] =
    execute(SentinelMasters)

  def sentinelMaster(masterName: String): Result[BMap[String, String]] =
    execute(new SentinelMaster(masterName))

  def sentinelSlaves(masterName: String): Result[Seq[BMap[String, String]]] =
    execute(new SentinelSlaves(masterName))

  def sentinelSentinels(masterName: String): Result[Seq[BMap[String, String]]] =
    execute(new SentinelSentinels(masterName))

  def sentinelGetMasterAddrByName(masterName: String): Result[NodeAddress] =
    execute(new SentinelGetMasterAddrByName(masterName))

  def sentinelFailover(masterName: String): Result[Unit] =
    execute(new SentinelFailover(masterName))

  private object SentinelMasters
    extends AbstractRedisCommand(multiBulkAsSeq(flatMultiBulkAsMapOf[String, String])) with NodeCommand {
    val encoded: Encoded = encoder("SENTINEL", "masters").result
  }

  private final class SentinelMaster(masterName: String)
    extends AbstractRedisCommand(flatMultiBulkAsMapOf[String, String]) with NodeCommand {
    val encoded: Encoded = encoder("SENTINEL", "master").add(masterName).result
  }

  private final class SentinelSlaves(masterName: String)
    extends AbstractRedisCommand(multiBulkAsSeq(flatMultiBulkAsMapOf[String, String])) with NodeCommand {
    val encoded: Encoded = encoder("SENTINEL", "slaves").add(masterName).result
  }

  private final class SentinelSentinels(masterName: String)
    extends AbstractRedisCommand(multiBulkAsSeq(flatMultiBulkAsMapOf[String, String])) with NodeCommand {
    val encoded: Encoded = encoder("SENTINEL", "sentinels").add(masterName).result
  }

  private final class SentinelGetMasterAddrByName(masterName: String)
    extends AbstractRedisCommand(multiBulkAsNodeAddress) with NodeCommand {
    val encoded: Encoded = encoder("SENTINEL", "get-master-addr-by-name").add(masterName).result
  }

  private final class SentinelFailover(masterName: String) extends RedisUnitCommand with NodeCommand {
    def encoded: Encoded = encoder("SENTINEL", "failover").add(masterName).result
  }
}
