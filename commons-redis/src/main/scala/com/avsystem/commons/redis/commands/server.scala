package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis.Scope.Node
import com.avsystem.commons.redis.{ClusteredApiSubset, NodeApiSubset, RedisUnitCommand, Unkeyed}

trait ClusteredServerApi extends ClusteredApiSubset

trait NodeServerApi extends ClusteredServerApi with NodeApiSubset {
  def flushall: Result[Unit, Node] =
    execute(Flushall)
}

case object Flushall extends RedisUnitCommand[Node] with Unkeyed {
  def encode = encoder("FLUSHALL").result
}
