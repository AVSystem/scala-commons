package com.avsystem.commons
package redis

import akka.util.Timeout
import com.avsystem.commons.redis.ApiSubset.{HeadOps, IterableTailOps, IteratorTailOps}
import com.avsystem.commons.redis.commands._
import com.avsystem.commons.redis.util.{HeadIterable, HeadIterator, SingletonSeq}

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.concurrent.{Await, Future}

trait ApiSubset { self =>
  type Input[A] >: RedisCommand[A]

  /**
    * The type constructor into which a result of each command is wrapped. For example if `Result` is
    * [[scala.concurrent.Future Future]], then [[commands.StringsApi.incr incr]] returns `Future[Long]`.
    */
  type Result[A]

  /**
    * The type of Redis keys or key patterns used in methods representing Redis commands. For example, if `Key = String`
    * then [[commands.StringsApi.get get]] returns `Result[Opt[String]]`. This type is used only for toplevel Redis keys, hash
    * keys have their own type, [[HashKey]].
    */
  type Key
  /**
    * The type of Redis hash keys or hash key patterns used in methods representing Redis commands that work on
    * hashes ([[commands.HashesApi HashesApi]]).
    */
  type HashKey
  /**
    * The type of Redis values used in methods representing Redis commands. "Value" is the data that might be
    * stored directly under a Redis key (e.g. using [[commands.StringsApi.set set]]) but also a value of hash field, list element,
    * set member, sorted set member, geo set member or element inserted into hyper-log-log structure.
    * There are no separate types specified for every of those use cases because only one of them can be used in a
    * single command (for example, there is no Redis command that works on both list elements and set members
    * at the same time).
    */
  type Value

  protected implicit def keyCodec: RedisDataCodec[Key]
  protected implicit def hashKeyCodec: RedisDataCodec[HashKey]
  protected implicit def valueCodec: RedisDataCodec[Value]

  def execute[A](command: Input[A]): Result[A]

  protected implicit def iterableTailOps[T](tail: Iterable[T]): IterableTailOps[T] = new IterableTailOps(tail)
  protected implicit def iteratorTailOps[T](tail: Iterator[T]): IteratorTailOps[T] = new IteratorTailOps(tail)
  protected implicit def headOps[T](head: T): HeadOps[T] = new HeadOps(head)
}

object ApiSubset {
  class HeadOps[A](private val head: A) extends AnyVal {
    def single: Seq[A] = new SingletonSeq[A](head)
  }
  class IterableTailOps[A](private val tail: Iterable[A]) extends AnyVal {
    def +::(head: A): Iterable[A] = new HeadIterable(head, tail)
  }
  class IteratorTailOps[A](private val tail: Iterator[A]) extends AnyVal {
    def +::(head: A): Iterator[A] = new HeadIterator(head, tail)
  }
}

trait RecoverableApiSubset extends ApiSubset {
  protected def recoverWith[A](executed: => Result[A])(fun: PartialFunction[Throwable, Result[A]]): Result[A]
}

trait RedisRawApi extends ApiSubset {
  type Input[A] = RedisCommand[A]
  type Result[A] = RawCommand
  def execute[A](command: RedisCommand[A]) = command
}

trait RedisBatchApi extends ApiSubset {
  type Input[A] = RedisBatch[A]
  type Result[A] = RedisBatch[A]
  def execute[A](command: RedisBatch[A]) = command
}

trait RedisExecutedApi extends RecoverableApiSubset {
  type Input[A] = RedisBatch[A]
  def timeout: Timeout
  def executor: RedisExecutor
  def executeAsync[A](command: RedisBatch[A]): Future[A] =
    executor.executeBatch(command)(timeout)
}

trait RedisAsyncApi extends RedisExecutedApi {
  type Result[A] = Future[A]
  def execute[A](command: RedisBatch[A]) = executeAsync(command)
  def recoverWith[A](executed: => Future[A])(fun: PartialFunction[Throwable, Future[A]]) =
    executed.recoverWith(fun)(executor.executionContext)
}

trait RedisBlockingApi extends RedisExecutedApi {
  type Result[A] = A
  def execute[A](command: RedisBatch[A]) = Await.result(executeAsync(command), Duration.Inf)
  def recoverWith[A](executed: => A)(fun: PartialFunction[Throwable, A]) =
    try executed catch fun
}

trait RedisKeyedApi extends AnyRef
  with KeyedKeysApi
  with StringsApi
  with KeyedClusterApi
  with GeoApi
  with KeyedScriptingApi
  with HashesApi
  with SortedSetsApi
  with ListsApi
  with SetsApi
  with HyperLogLogApi

trait RedisRecoverableKeyedApi extends RedisKeyedApi
  with RecoverableKeyedScriptingApi

trait RedisNodeApi extends RedisKeyedApi
  with NodeKeysApi
  with NodeServerApi
  with NodeClusterApi
  with NodeConnectionApi
  with NodeScriptingApi

trait RedisRecoverableNodeApi extends RedisRecoverableKeyedApi with RedisNodeApi

trait RedisOperationApi extends RedisNodeApi
  with TransactionApi

trait RedisConnectionApi extends RedisOperationApi
  with ConnectionClusterApi
  with ConnectionConnectionApi
  with ConnectionServerApi
  with ConnectionScriptingApi
  with BlockingListsApi

trait RedisRecoverableConnectionApi extends RedisRecoverableNodeApi with RedisConnectionApi
