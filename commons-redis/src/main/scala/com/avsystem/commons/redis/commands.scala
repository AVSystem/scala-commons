package com.avsystem.commons
package redis

import akka.util.{ByteString, Timeout}
import com.avsystem.commons.redis.ApiSubset.{HeadOps, IterableTailOps, IteratorTailOps}
import com.avsystem.commons.redis.commands._
import com.avsystem.commons.redis.util.{ByteStringCodec, HeadIterable, HeadIterator, SingletonSeq}
import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec}

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.concurrent.{Await, ExecutionContext, Future}

trait RedisExecutor {
  implicit def executionContext: ExecutionContext
  def executeBatch[A](batch: RedisBatch[A])(implicit timeout: Timeout): Future[A]
}
trait RedisOpExecutor {
  def executeOp[A](op: RedisOp[A])(implicit timeout: Timeout): Future[A]
}

trait RedisKeyedExecutor extends RedisExecutor
trait RedisNodeExecutor extends RedisKeyedExecutor with RedisOpExecutor
trait RedisConnectionExecutor extends RedisNodeExecutor

case class RedisDataCodec[T](read: ByteString => T, write: T => ByteString)
object RedisDataCodec extends LowPriorityRedisDataCodecs {
  def apply[T](implicit rdc: RedisDataCodec[T]): RedisDataCodec[T] = rdc

  def write[T](value: T)(implicit rdc: RedisDataCodec[T]): ByteString = rdc.write(value)
  def read[T](raw: ByteString)(implicit rdc: RedisDataCodec[T]): T = rdc.read(raw)

  implicit val byteStringCodec: RedisDataCodec[ByteString] = RedisDataCodec(identity, identity)
  implicit val byteArrayCodec: RedisDataCodec[Array[Byte]] = RedisDataCodec(_.toArray, ByteString(_))
  implicit def fromKeyCodec[T: GenKeyCodec]: RedisDataCodec[T] =
    RedisDataCodec(bytes => GenKeyCodec.read(bytes.utf8String), value => ByteString(GenKeyCodec.write(value)))
}
trait LowPriorityRedisDataCodecs { this: RedisDataCodec.type =>
  implicit def fromGenCodec[T: GenCodec]: RedisDataCodec[T] =
    RedisDataCodec(bytes => ByteStringCodec.read(bytes), value => ByteStringCodec.write(value))
}

trait ApiSubset { self =>
  type Input[A] >: RedisCommand[A]
  type Result[A]

  type Key
  type HashKey
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
  with KeyedServerApi
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

trait RedisRecoverableConnectionApi extends RedisRecoverableNodeApi with RedisConnectionApi

abstract class AbstractRedisApi[Self[K0, H0, V0] <: AbstractRedisApi[Self, K0, H0, V0], K, H, V](implicit
  val keyCodec: RedisDataCodec[K], val hashKeyCodec: RedisDataCodec[H], val valueCodec: RedisDataCodec[V])
  extends ApiSubset {
  type Key = K
  type HashKey = H
  type Value = V

  type WithKey[K0] = Self[K0, H, V]
  type WithHashKey[H0] = Self[K, H0, V]
  type WithValue[V0] = Self[K, H, V0]

  def keyType[K0: RedisDataCodec]: WithKey[K0] =
    copy(newKeyCodec = RedisDataCodec[K0])

  def transformKey[K0](read: K => K0)(write: K0 => K): WithKey[K0] =
    copy(newKeyCodec = RedisDataCodec(keyCodec.read andThen read, write andThen keyCodec.write))

  def hashKeyType[H0: RedisDataCodec]: WithHashKey[H0] =
    copy(newHashKeyCodec = RedisDataCodec[H0])

  def transformHashKey[H0](read: H => H0)(write: H0 => H): WithHashKey[H0] =
    copy(newHashKeyCodec = RedisDataCodec(hashKeyCodec.read andThen read, write andThen hashKeyCodec.write))

  def valueType[V0: RedisDataCodec]: WithValue[V0] =
    copy(newValueCodec = RedisDataCodec[V0])

  def transformValue[V0](read: V => V0)(write: V0 => V): WithValue[V0] =
    copy(newValueCodec = RedisDataCodec(valueCodec.read andThen read, write andThen valueCodec.write))

  def copy[K0, H0, V0](
    newKeyCodec: RedisDataCodec[K0] = keyCodec,
    newHashKeyCodec: RedisDataCodec[H0] = hashKeyCodec,
    newValueCodec: RedisDataCodec[V0] = valueCodec
  ): Self[K0, H0, V0]
}

object RedisApi {
  class Raw[Key: RedisDataCodec, HashKey: RedisDataCodec, Value: RedisDataCodec]
    extends AbstractRedisApi[Raw, Key, HashKey, Value] with RedisConnectionApi with RedisRawApi {
    def copy[K, H, V](newKeyCodec: RedisDataCodec[K], newHashKeyCodec: RedisDataCodec[H], newValueCodec: RedisDataCodec[V]) =
      new Raw[K, H, V]()(newKeyCodec, newHashKeyCodec, newValueCodec)
  }
  object Raw {
    object StringTyped extends Raw[String, String, String]
    object BinaryTyped extends Raw[ByteString, ByteString, ByteString]
  }

  class Batches[Key: RedisDataCodec, HashKey: RedisDataCodec, Value: RedisDataCodec]
    extends AbstractRedisApi[Batches, Key, HashKey, Value] with RedisConnectionApi with RedisBatchApi {
    def copy[K, H, V](newKeyCodec: RedisDataCodec[K], newHashKeyCodec: RedisDataCodec[H], newValueCodec: RedisDataCodec[V]) =
      new Batches[K, H, V]()(newKeyCodec, newHashKeyCodec, newValueCodec)
  }
  object Batches {
    object StringTyped extends Batches[String, String, String]
    object BinaryTyped extends Batches[ByteString, ByteString, ByteString]
  }

  object Keyed {
    class Async[Key: RedisDataCodec, HashKey: RedisDataCodec, Value: RedisDataCodec](val executor: RedisKeyedExecutor)
      (implicit val timeout: Timeout) extends AbstractRedisApi[Async, Key, HashKey, Value] with RedisRecoverableKeyedApi with RedisAsyncApi {
      def copy[K, H, V](newKeyCodec: RedisDataCodec[K], newHashKeyCodec: RedisDataCodec[H], newValueCodec: RedisDataCodec[V]) =
        new Async[K, H, V](executor)(newKeyCodec, newHashKeyCodec, newValueCodec, timeout)
    }
    object Async {
      case class StringTyped(exec: RedisKeyedExecutor)(implicit timeout: Timeout) extends Async[String, String, String](exec)
      case class BinaryTyped(exec: RedisKeyedExecutor)(implicit timeout: Timeout) extends Async[ByteString, ByteString, ByteString](exec)
    }

    class Blocking[Key: RedisDataCodec, HashKey: RedisDataCodec, Value: RedisDataCodec](val executor: RedisKeyedExecutor)
      (implicit val timeout: Timeout) extends AbstractRedisApi[Blocking, Key, HashKey, Value] with RedisRecoverableKeyedApi with RedisBlockingApi {
      def copy[K, H, V](newKeyCodec: RedisDataCodec[K], newHashKeyCodec: RedisDataCodec[H], newValueCodec: RedisDataCodec[V]) =
        new Blocking[K, H, V](executor)(newKeyCodec, newHashKeyCodec, newValueCodec, timeout)
    }
    object Blocking {
      case class StringTyped(exec: RedisKeyedExecutor)(implicit timeout: Timeout) extends Blocking[String, String, String](exec)
      case class BinaryTyped(exec: RedisKeyedExecutor)(implicit timeout: Timeout) extends Blocking[ByteString, ByteString, ByteString](exec)
    }
  }

  object Node {
    class Async[Key: RedisDataCodec, HashKey: RedisDataCodec, Value: RedisDataCodec](val executor: RedisNodeExecutor)
      (implicit val timeout: Timeout) extends AbstractRedisApi[Async, Key, HashKey, Value] with RedisRecoverableNodeApi with RedisAsyncApi {
      def copy[K, H, V](newKeyCodec: RedisDataCodec[K], newHashKeyCodec: RedisDataCodec[H], newValueCodec: RedisDataCodec[V]) =
        new Async[K, H, V](executor)(newKeyCodec, newHashKeyCodec, newValueCodec, timeout)
    }
    object Async {
      case class StringTyped(exec: RedisNodeExecutor)(implicit timeout: Timeout) extends Async[String, String, String](exec)
      case class BinaryTyped(exec: RedisNodeExecutor)(implicit timeout: Timeout) extends Async[ByteString, ByteString, ByteString](exec)
    }

    class Blocking[Key: RedisDataCodec, HashKey: RedisDataCodec, Value: RedisDataCodec](val executor: RedisNodeExecutor)
      (implicit val timeout: Timeout) extends AbstractRedisApi[Blocking, Key, HashKey, Value] with RedisRecoverableNodeApi with RedisBlockingApi {
      def copy[K, H, V](newKeyCodec: RedisDataCodec[K], newHashKeyCodec: RedisDataCodec[H], newValueCodec: RedisDataCodec[V]) =
        new Blocking[K, H, V](executor)(newKeyCodec, newHashKeyCodec, newValueCodec, timeout)
    }
    object Blocking {
      case class StringTyped(exec: RedisNodeExecutor)(implicit timeout: Timeout) extends Blocking[String, String, String](exec)
      case class BinaryTyped(exec: RedisNodeExecutor)(implicit timeout: Timeout) extends Blocking[ByteString, ByteString, ByteString](exec)
    }
  }

  object Connection {
    class Async[Key: RedisDataCodec, HashKey: RedisDataCodec, Value: RedisDataCodec](val executor: RedisConnectionExecutor)
      (implicit val timeout: Timeout) extends AbstractRedisApi[Async, Key, HashKey, Value] with RedisRecoverableConnectionApi with RedisAsyncApi {
      def copy[K, H, V](newKeyCodec: RedisDataCodec[K], newHashKeyCodec: RedisDataCodec[H], newValueCodec: RedisDataCodec[V]) =
        new Async[K, H, V](executor)(newKeyCodec, newHashKeyCodec, newValueCodec, timeout)
    }
    object Async {
      case class StringTyped(exec: RedisConnectionExecutor)(implicit timeout: Timeout) extends Async[String, String, String](exec)
      case class BinaryTyped(exec: RedisConnectionExecutor)(implicit timeout: Timeout) extends Async[ByteString, ByteString, ByteString](exec)
    }

    class Blocking[Key: RedisDataCodec, HashKey: RedisDataCodec, Value: RedisDataCodec](val executor: RedisConnectionExecutor)
      (implicit val timeout: Timeout) extends AbstractRedisApi[Blocking, Key, HashKey, Value] with RedisRecoverableConnectionApi with RedisBlockingApi {
      def copy[K, H, V](newKeyCodec: RedisDataCodec[K], newHashKeyCodec: RedisDataCodec[H], newValueCodec: RedisDataCodec[V]) =
        new Blocking[K, H, V](executor)(newKeyCodec, newHashKeyCodec, newValueCodec, timeout)
    }
    object Blocking {
      case class StringTyped(exec: RedisConnectionExecutor)(implicit timeout: Timeout) extends Blocking[String, String, String](exec)
      case class BinaryTyped(exec: RedisConnectionExecutor)(implicit timeout: Timeout) extends Blocking[ByteString, ByteString, ByteString](exec)
    }
  }
}
