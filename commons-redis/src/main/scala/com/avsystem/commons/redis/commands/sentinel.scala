package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis.commands.ReplyDecoders._
import com.avsystem.commons.redis.{AbstractRedisCommand, ApiSubset, NodeAddress, NodeCommand}

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

  private object SentinelMasters
    extends AbstractRedisCommand(multiBulkSeq(flatMultiBulkMap[String, String])) with NodeCommand {
    val encoded: Encoded = encoder("SENTINEL", "masters").result
  }

  private final class SentinelMaster(masterName: String)
    extends AbstractRedisCommand(flatMultiBulkMap[String, String]) with NodeCommand {
    val encoded: Encoded = encoder("SENTINEL", "master").add(masterName).result
  }

  private final class SentinelSlaves(masterName: String)
    extends AbstractRedisCommand(multiBulkSeq(flatMultiBulkMap[String, String])) with NodeCommand {
    val encoded: Encoded = encoder("SENTINEL", "slaves").add(masterName).result
  }

  private final class SentinelSentinels(masterName: String)
    extends AbstractRedisCommand(multiBulkSeq(flatMultiBulkMap[String, String])) with NodeCommand {
    val encoded: Encoded = encoder("SENTINEL", "slaves").add(masterName).result
  }

  private final class SentinelGetMasterAddrByName(masterName: String)
    extends AbstractRedisCommand(multiBulkNodeAddress) with NodeCommand {
    val encoded: Encoded = encoder("SENTINEL", "get-master-addr-by-name").add(masterName).result
  }
}
