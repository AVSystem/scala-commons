package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis.Scope.Node
import com.avsystem.commons.redis.{ClusterApiSubset, NodeApiSubset, RedisUnitCommand, Unkeyed}

trait ClusterServerApi extends ClusterApiSubset

trait NodeServerApi extends ClusterServerApi with NodeApiSubset {
  def flushall: Result[Unit, Node] =
    execute(Flushall)
}

case object Flushall extends RedisUnitCommand[Node] with Unkeyed {
  def encode = encoder("FLUSHALL").result
}
