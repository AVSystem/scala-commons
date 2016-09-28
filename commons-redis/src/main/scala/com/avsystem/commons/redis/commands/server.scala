package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis.{ApiSubset, NodeCommand, RedisUnitCommand}

trait ClusteredServerApi extends ApiSubset

trait NodeServerApi extends ClusteredServerApi {
  def flushall: Result[Unit] =
    execute(Flushall)

  private case object Flushall extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("FLUSHALL").result
  }
}
