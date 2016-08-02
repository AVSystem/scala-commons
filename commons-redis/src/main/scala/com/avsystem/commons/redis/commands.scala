package com.avsystem.commons
package redis

import com.avsystem.commons.redis.commands._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

trait RedisExecutor {
  def execute[A](batch: RedisBatch[A]): Future[A]
}

/*
 * These three subtraits provide rudimentary typesafety layer.
 * Their sole purpose is so that e.g. `RedisNodeClient.toExecutor` cannot be passed into `RedisNodeAsyncCommands`.
 * However, it's still easy to circumvent this protection by using client's `executeBatch` method directly or
 * by implementing `RedisConnectionExecutor` manually.
 */
trait RedisClusteredExecutor extends RedisExecutor
trait RedisNodeExecutor extends RedisClusteredExecutor
trait RedisConnectionExecutor extends RedisNodeExecutor

trait ApiSubset {self =>
  type Result[A]
  protected def execute[A](cmd: RedisCommand[A]): Result[A]
}

trait CommandSubset extends ApiSubset {
  type Result[A] = RedisBatch[A]
  protected def execute[A](cmd: RedisCommand[A]) = cmd
}
trait AsyncCommandSubset extends ApiSubset {
  type Result[A] = Future[A]
  protected def executor: RedisClusteredExecutor
  protected def execute[A](cmd: RedisCommand[A]) = executor.execute(cmd)
}
trait BlockingCommandSubset extends ApiSubset {
  type Result[A] = A
  protected def timeout: Duration
  protected def executor: RedisClusteredExecutor
  protected def execute[A](cmd: RedisCommand[A]) = Await.result(executor.execute(cmd), timeout)
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

object RedisCommands extends RedisConnectionApi with CommandSubset

case class RedisClusteredAsyncCommands(executor: RedisClusteredExecutor)
  extends RedisClusteredApi with AsyncCommandSubset
case class RedisNodeAsyncCommands(executor: RedisNodeExecutor)
  extends RedisNodeApi with AsyncCommandSubset
case class RedisConnectionAsyncCommands(executor: RedisConnectionExecutor)
  extends RedisConnectionApi with AsyncCommandSubset

case class RedisClusteredBlockingCommands(executor: RedisClusteredExecutor, timeout: Duration)
  extends RedisClusteredApi with BlockingCommandSubset
case class RedisNodeBlockingCommands(executor: RedisNodeExecutor, timeout: Duration)
  extends RedisNodeApi with BlockingCommandSubset
case class RedisConnectionBlockingCommands(executor: RedisConnectionExecutor, timeout: Duration)
  extends RedisConnectionApi with BlockingCommandSubset
