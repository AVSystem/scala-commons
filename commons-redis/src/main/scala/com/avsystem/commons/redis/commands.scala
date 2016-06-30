package com.avsystem.commons
package redis

import com.avsystem.commons.redis.Scope.{Cluster, Connection, Node}
import com.avsystem.commons.redis.commands._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

trait RedisExecutor[+S] {
  def execute[A](batch: RedisBatch[A, S]): Future[A]
}

trait ApiSubset {self =>
  type CmdScope
  type Result[A, S]

  protected def execute[A, S >: CmdScope](cmd: RedisCommand[A, S]): Result[A, S]
}

trait ClusteredApiSubset extends ApiSubset {
  type CmdScope <: Scope.Cluster
}
trait NodeApiSubset extends ClusteredApiSubset {
  type CmdScope <: Scope.Node
}
trait OperationApiSubset extends NodeApiSubset {
  type CmdScope <: Scope.Operation
}
trait ConnectionApiSubset extends OperationApiSubset {
  type CmdScope <: Scope.Connection
}

trait CommandSubset extends ApiSubset {
  type Result[A, S] = RedisCommand[A, S]
  protected def execute[A, S >: CmdScope](cmd: RedisCommand[A, S]) = cmd
}
trait AsyncCommandSubset extends ApiSubset {
  type Result[A, S] = Future[A]
  protected def executor: RedisExecutor[CmdScope]
  protected def execute[A, S >: CmdScope](cmd: RedisCommand[A, S]) = executor.execute(cmd)
}
trait BlockingCommandSubset extends ApiSubset {
  type Result[A, S] = A
  protected def timeout: Duration
  protected def executor: RedisExecutor[CmdScope]
  protected def execute[A, S >: CmdScope](cmd: RedisCommand[A, S]) = Await.result(executor.execute(cmd), timeout)
}

trait RedisClusteredApi extends AnyRef
  with ClusteredKeysApi
  with StringsApi
  with ClusteredServerApi
  with ClusteredClusterApi

trait RedisNodeApi extends RedisClusteredApi
  with NodeKeysApi
  with NodeServerApi
  with NodeClusterApi
  with NodeConnectionApi

trait RedisOperationApi extends RedisNodeApi
  with TransactionApi

trait RedisConnectionApi extends RedisOperationApi
  with ConnectionClusterApi
  with ConnectionConnectionApi

trait AbstractRedisClusteredApi extends RedisClusteredApi {self =>
  type CmdScope = Scope.Cluster
}
trait AbstractRedisNodeApi extends RedisNodeApi {self =>
  type CmdScope = Scope.Node
}
trait AbstractRedisConnectionApi extends RedisConnectionApi {self =>
  type CmdScope = Scope.Connection
}

object RedisCommands extends AbstractRedisConnectionApi with CommandSubset

case class RedisClusteredAsyncCommands(executor: RedisExecutor[Cluster])
  extends AbstractRedisClusteredApi with AsyncCommandSubset
case class RedisNodeAsyncCommands(executor: RedisExecutor[Node])
  extends AbstractRedisNodeApi with AsyncCommandSubset
case class RedisConnectionAsyncCommands(executor: RedisExecutor[Connection])
  extends AbstractRedisConnectionApi with AsyncCommandSubset

case class RedisClusteredBlockingCommands(executor: RedisExecutor[Cluster], timeout: Duration)
  extends AbstractRedisClusteredApi with BlockingCommandSubset
case class RedisNodeBlockingCommands(executor: RedisExecutor[Node], timeout: Duration)
  extends AbstractRedisNodeApi with BlockingCommandSubset
case class RedisConnectionBlockingCommands(executor: RedisExecutor[Connection], timeout: Duration)
  extends AbstractRedisConnectionApi with BlockingCommandSubset
