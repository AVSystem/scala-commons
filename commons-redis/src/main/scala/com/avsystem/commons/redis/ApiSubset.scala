package com.avsystem.commons
package redis

import akka.util.ByteString
import com.avsystem.commons.redis.ApiSubset.{HeadOps, IterableTailOps, IteratorTailOps}
import com.avsystem.commons.redis.commands._
import com.avsystem.commons.redis.config.ExecutionConfig
import com.avsystem.commons.redis.util.{HeadIterable, HeadIterator, SingletonSeq}

import scala.concurrent.Await
import scala.concurrent.duration._

/**
  * Encapsulates types that Redis commands may be parameterized with and provides serialization for these types.
  */
trait RedisSerialization {
  /**
    * The type of Redis keys or key patterns used in methods representing Redis commands. For example, if `Key = String`
    * then [[commands.StringsApi.get get]] returns `Result[Opt[String]]`. This type is used only for toplevel Redis keys,
    * hash keys have their own type, [[Field]]
    */
  type Key

  /**
    * The type of Redis hash keys or hash key patterns used in methods representing Redis commands that work on
    * hashes ([[commands.HashesApi HashesApi]]).
    */
  type Field

  /**
    * The type of Redis values used in methods representing Redis commands. "Value" is the data that might be
    * stored directly under a Redis key (e.g. using [[commands.StringsApi.set set]]) but also a value of hash field,
    * list element, set member, sorted set member, geo set member or element inserted into hyper-log-log structure.
    * There are no separate types specified for every of those use cases because only one of them can be used in a
    * single command (for example, there is no Redis command that works on both list elements and set members
    * at the same time).
    */
  type Value

  /**
    * Type used to represent key-value pair sequences, primarily entries in [[StreamsApi]] but also hashes in
    * [[HashesApi]]. `Record` might be a "raw" type like `Map[Key, Value]` or `Seq[(Key, Value)]` but normally
    * `Record` is customized to be an ADT - a case class or sealed hierarchy.
    */
  type Record

  def keyCodec: RedisDataCodec[Key]
  def fieldCodec: RedisDataCodec[Field]
  def valueCodec: RedisDataCodec[Value]
  def recordCodec: RedisRecordCodec[Record]

  type WithKey[K] = RedisSerialization.Aux[K, Field, Value, Record]
  type WithField[F] = RedisSerialization.Aux[Key, F, Value, Record]
  type WithValue[V] = RedisSerialization.Aux[Key, Field, V, Record]
  type WithRecord[R] = RedisSerialization.Aux[Key, Field, Value, R]

  def keyType[K0: RedisDataCodec]: WithKey[K0]
  def fieldType[F0: RedisDataCodec]: WithField[F0]
  def valueType[V0: RedisDataCodec]: WithValue[V0]
  def recordType[R0: RedisRecordCodec]: WithRecord[R0]
}

object RedisSerialization {
  type Aux[K, F, V, R] = RedisSerialization {
    type Key = K
    type Field = F
    type Value = V
    type Record = R
  }

  object Strings extends Generic[String, String, String, Map[String, String]]
  object ByteStrings extends Generic[ByteString, ByteString, ByteString, Map[ByteString, ByteString]]

  class Generic[K, F, V, R](implicit
    val keyCodec: RedisDataCodec[K],
    val fieldCodec: RedisDataCodec[F],
    val valueCodec: RedisDataCodec[V],
    val recordCodec: RedisRecordCodec[R]
  ) extends RedisSerialization {
    type Key = K
    type Field = F
    type Value = V
    type Record = R

    def keyType[K0: RedisDataCodec]: WithKey[K0] = new Generic
    def fieldType[F0: RedisDataCodec]: WithField[F0] = new Generic
    def valueType[V0: RedisDataCodec]: WithValue[V0] = new Generic
    def recordType[R0: RedisRecordCodec]: WithRecord[R0] = new Generic
  }
}

trait ApiSubset { self =>
  val serialization: RedisSerialization

  type Key = serialization.Key
  type Field = serialization.Field
  type Value = serialization.Value
  type Record = serialization.Record

  protected implicit final def keyCodec: RedisDataCodec[Key] = serialization.keyCodec
  protected implicit final def fieldCodec: RedisDataCodec[Field] = serialization.fieldCodec
  protected implicit final def valueCodec: RedisDataCodec[Value] = serialization.valueCodec
  protected implicit final def recordCodec: RedisRecordCodec[Record] = serialization.recordCodec

  /**
    * The type constructor into which a result of each command is wrapped. For example if `Result` is
    * `Future`, then [[commands.StringsApi.incr incr]] returns `Future[Long]`.
    */
  type Result[A]

  def execute[A](command: RedisCommand[A]): Result[A]

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
  type Result[A] = RawCommand
  def execute[A](command: RedisCommand[A]): RedisCommand[A] = command
}

trait RedisBatchApi extends ApiSubset {
  type Result[A] = RedisBatch[A]
  def execute[A](command: RedisCommand[A]): RedisBatch[A] = command.batchOrFallback
}

trait RedisExecutedApi extends RecoverableApiSubset {
  def execConfig: ExecutionConfig
  def executor: RedisExecutor
  def executeAsync[A](command: RedisCommand[A]): Future[A] =
    executor.executeBatch(command.batchOrFallback, execConfig)
}

trait RedisAsyncApi extends RedisExecutedApi {
  type Result[A] = Future[A]
  def execute[A](command: RedisCommand[A]): Future[A] = executeAsync(command)
  def recoverWith[A](executed: => Future[A])(fun: PartialFunction[Throwable, Future[A]]): Future[A] =
    executed.recoverWith(fun)(executor.executionContext)
}

trait RedisBlockingApi extends RedisExecutedApi {
  type Result[A] = A
  def execute[A](command: RedisCommand[A]): A =
  // executeAsync should already handle timeouts, but just to be safe let's pass the standard timeout plus one second
    Await.result(executeAsync(command), execConfig.responseTimeout.duration + 1.second)
  def recoverWith[A](executed: => A)(fun: PartialFunction[Throwable, A]): A =
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
  with StreamsApi

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

trait RedisRecoverableConnectionApi extends RedisRecoverableNodeApi with RedisConnectionApi
