package com.avsystem.commons
package redis

import com.avsystem.commons.redis.Scope.{Cluster, Connection, Node, Operation}
import com.avsystem.commons.redis.commands.{ClusterKeysApi, ClusterServerApi, NodeKeysApi, NodeServerApi, StringsApi, TransactionApi}

import scala.concurrent.Future

trait RedisExecutor[+S] {
  def execute[A](batch: RedisBatch[A, S]): Future[A]
}

trait ApiSubset {self =>
  type CmdScope
  type Result[+A, -S]

  protected def execute[A, S >: CmdScope](cmd: RedisCommand[A, S]): Result[A, S]
}

trait ClusterApiSubset extends ApiSubset {
  type CmdScope <: Scope.Cluster
}
trait NodeApiSubset extends ClusterApiSubset {
  type CmdScope <: Scope.Node
}
trait OperationApiSubset extends NodeApiSubset {
  type CmdScope <: Scope.Operation
}
trait ConnectionApiSubset extends OperationApiSubset {
  type CmdScope <: Scope.Connection
}

trait CommandSubset extends ApiSubset {
  type Result[+A, -S] = RedisCommand[A, S]
  protected def execute[A, S >: CmdScope](cmd: RedisCommand[A, S]) = cmd
}
trait FutureSubset extends ApiSubset {
  type Result[+A, -S] = Future[A]
  protected def executor: RedisExecutor[CmdScope]
  protected def execute[A, S >: CmdScope](cmd: RedisCommand[A, S]) = executor.execute(cmd)
}

trait RedisClusterApi extends ClusterKeysApi with StringsApi with ClusterServerApi
trait RedisNodeApi extends RedisClusterApi with NodeKeysApi with NodeServerApi
trait RedisOperationApi extends RedisNodeApi with TransactionApi
trait RedisConnectionApi extends RedisOperationApi

trait AbstractRedisClusterApi extends RedisClusterApi {self =>
  type CmdScope = Scope.Cluster
}
object RedisClusterCommands extends AbstractRedisClusterApi with CommandSubset
case class RedisClusterFutures(executor: RedisExecutor[Cluster]) extends AbstractRedisClusterApi with FutureSubset

trait AbstractRedisNodeApi extends RedisNodeApi {self =>
  type CmdScope = Scope.Node
}
object RedisNodeCommands extends AbstractRedisNodeApi with CommandSubset
case class RedisNodeFutures(executor: RedisExecutor[Node]) extends AbstractRedisNodeApi with FutureSubset

trait AbstractRedisOperationApi extends RedisOperationApi {self =>
  type CmdScope = Scope.Operation
}
object RedisOperationCommands extends AbstractRedisOperationApi with CommandSubset
case class RedisOperationFutures(executor: RedisExecutor[Operation]) extends AbstractRedisOperationApi with FutureSubset

trait AbstractRedisConnectionApi extends RedisConnectionApi {self =>
  type CmdScope = Scope.Connection
}
object RedisConnectionCommands extends AbstractRedisConnectionApi with CommandSubset
case class RedisConnectionFutures(executor: RedisExecutor[Connection]) extends AbstractRedisConnectionApi with FutureSubset
