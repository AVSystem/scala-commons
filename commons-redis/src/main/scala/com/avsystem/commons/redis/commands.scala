package com.avsystem.commons
package redis

import akka.util.{ByteString, Timeout}
import com.avsystem.commons.redis.commands._
import com.avsystem.commons.redis.util.ByteStringCodec
import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec}

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.concurrent.{Await, ExecutionContext, Future}

trait RedisExecutor {
  def executionContext: ExecutionContext
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
  // type function that changes key, hash key and value type of self-type
  type Self[K, H, V] <: AbstractApiSubset[K, H, V]
  type Result[A]

  type Key
  type HashKey
  type Value

  type WithKey[K] = Self[K, HashKey, Value]
  type WithHashKey[H] = Self[Key, H, Value]
  type WithValue[V] = Self[Key, HashKey, V]

  def keyType[K: RedisDataCodec]: WithKey[K] =
    copy(newKeyCodec = RedisDataCodec[K])

  def transformKey[K](read: Key => K)(write: K => Key): WithKey[K] =
    copy(newKeyCodec = RedisDataCodec(keyCodec.read andThen read, write andThen keyCodec.write))

  def hashKeyType[H: RedisDataCodec]: WithHashKey[H] =
    copy(newHashKeyCodec = RedisDataCodec[H])

  def transformHashKey[H](read: HashKey => H)(write: H => HashKey): WithHashKey[H] =
    copy(newHashKeyCodec = RedisDataCodec(hashKeyCodec.read andThen read, write andThen hashKeyCodec.write))

  def valueType[V: RedisDataCodec]: WithValue[V] =
    copy(newValueCodec = RedisDataCodec[V])

  def transformValue[V](read: Value => V)(write: V => Value): WithValue[V] =
    copy(newValueCodec = RedisDataCodec(valueCodec.read andThen read, write andThen valueCodec.write))

  protected implicit def keyCodec: RedisDataCodec[Key]
  protected implicit def hashKeyCodec: RedisDataCodec[HashKey]
  protected implicit def valueCodec: RedisDataCodec[Value]

  protected def copy[K, H, V](
    newKeyCodec: RedisDataCodec[K] = keyCodec,
    newHashKeyCodec: RedisDataCodec[H] = hashKeyCodec,
    newValueCodec: RedisDataCodec[V] = valueCodec
  ): Self[K, H, V]

  protected def execute[A](command: RedisCommand[A]): Result[A]
}

abstract class AbstractApiSubset[K, H, V](implicit
  val keyCodec: RedisDataCodec[K], val hashKeyCodec: RedisDataCodec[H], val valueCodec: RedisDataCodec[V])
  extends ApiSubset {
  type Key = K
  type HashKey = H
  type Value = V
}

trait RecoverableApiSubset extends ApiSubset {
  protected def recoverWith[A](executed: => Result[A])(fun: PartialFunction[Throwable, Result[A]]): Result[A]
}

trait RedisRawApi extends ApiSubset {
  type Result[A] = RawCommand
  type Self[K, H, V] <: AbstractApiSubset[K, H, V] with RedisRawApi
  protected def execute[A](command: RedisCommand[A]) = command
}
trait RedisBatchApi extends ApiSubset {
  type Result[A] = RedisBatch[A]
  type Self[K, H, V] <: AbstractApiSubset[K, H, V] with RedisBatchApi
  protected def execute[A](command: RedisCommand[A]) = command
}
trait RedisExecutedApi extends RecoverableApiSubset {
  protected def timeout: Timeout
  protected def executor: RedisExecutor
  protected def executeAsync[A](command: RedisCommand[A]): Future[A] =
    executor.executeBatch(command)(timeout)
}
trait RedisAsyncApi extends RedisExecutedApi {
  type Result[A] = Future[A]
  type Self[K, H, V] <: AbstractApiSubset[K, H, V] with RedisAsyncApi
  protected def execute[A](command: RedisCommand[A]) = executeAsync(command)
  protected def recoverWith[A](executed: => Future[A])(fun: PartialFunction[Throwable, Future[A]]) =
    executed.recoverWith(fun)(executor.executionContext)
}
trait RedisBlockingApi extends RedisExecutedApi {
  type Result[A] = A
  type Self[K, H, V] <: AbstractApiSubset[K, H, V] with RedisBlockingApi
  protected def execute[A](command: RedisCommand[A]) = Await.result(executeAsync(command), Duration.Inf)
  protected def recoverWith[A](executed: => A)(fun: PartialFunction[Throwable, A]) =
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
  with HyperLogLogApi {
  type Self[K, H, V] <: AbstractApiSubset[K, H, V] with RedisKeyedApi
}

trait RedisRecoverableKeyedApi extends RedisKeyedApi
  with RecoverableKeyedScriptingApi

trait RedisNodeApi extends RedisKeyedApi
  with NodeKeysApi
  with NodeServerApi
  with NodeClusterApi
  with NodeConnectionApi
  with NodeScriptingApi {
  type Self[K, H, V] <: AbstractApiSubset[K, H, V] with RedisNodeApi
}

trait RedisRecoverableNodeApi extends RedisRecoverableKeyedApi with RedisNodeApi

trait RedisOperationApi extends RedisNodeApi
  with TransactionApi {
  type Self[K, H, V] <: AbstractApiSubset[K, H, V] with RedisOperationApi
}

trait RedisConnectionApi extends RedisOperationApi
  with ConnectionClusterApi
  with ConnectionConnectionApi
  with ConnectionServerApi
  with ConnectionScriptingApi {
  type Self[K, H, V] <: AbstractApiSubset[K, H, V] with RedisConnectionApi
}

trait RedisRecoverableConnectionApi extends RedisRecoverableNodeApi with RedisConnectionApi

object RedisApi {
  class Raw[Key: RedisDataCodec, HashKey: RedisDataCodec, Value: RedisDataCodec]
    extends AbstractApiSubset[Key, HashKey, Value] with RedisConnectionApi with RedisRawApi {
    type Self[K, H, V] = Raw[K, H, V]
    protected def copy[K, H, V](newKeyCodec: RedisDataCodec[K], newHashKeyCodec: RedisDataCodec[H], newValueCodec: RedisDataCodec[V]) =
      new Raw[K, H, V]()(newKeyCodec, newHashKeyCodec, newValueCodec)
  }
  object Raw {
    object StringTyped extends Raw[String, String, String]
    object BinaryTyped extends Raw[ByteString, ByteString, ByteString]
  }

  class Batches[Key: RedisDataCodec, HashKey: RedisDataCodec, Value: RedisDataCodec]
    extends AbstractApiSubset[Key, HashKey, Value] with RedisConnectionApi with RedisBatchApi {
    type Self[K, H, V] = Batches[K, H, V]
    protected def copy[K, H, V](newKeyCodec: RedisDataCodec[K], newHashKeyCodec: RedisDataCodec[H], newValueCodec: RedisDataCodec[V]) =
      new Batches[K, H, V]()(newKeyCodec, newHashKeyCodec, newValueCodec)
  }
  object Batches {
    object StringTyped extends Batches[String, String, String]
    object BinaryTyped extends Batches[ByteString, ByteString, ByteString]
  }

  object Keyed {
    class Async[Key: RedisDataCodec, HashKey: RedisDataCodec, Value: RedisDataCodec](val executor: RedisKeyedExecutor)
      (implicit val timeout: Timeout) extends AbstractApiSubset[Key, HashKey, Value] with RedisRecoverableKeyedApi with RedisAsyncApi {
      type Self[K, H, V] = Async[K, H, V]
      protected def copy[K, H, V](newKeyCodec: RedisDataCodec[K], newHashKeyCodec: RedisDataCodec[H], newValueCodec: RedisDataCodec[V]) =
        new Async[K, H, V](executor)(newKeyCodec, newHashKeyCodec, newValueCodec, timeout)
    }
    object Async {
      case class StringTyped(exec: RedisKeyedExecutor)(implicit timeout: Timeout) extends Async[String, String, String](exec)
      case class BinaryTyped(exec: RedisKeyedExecutor)(implicit timeout: Timeout) extends Async[ByteString, ByteString, ByteString](exec)
    }

    class Blocking[Key: RedisDataCodec, HashKey: RedisDataCodec, Value: RedisDataCodec](val executor: RedisKeyedExecutor)
      (implicit val timeout: Timeout) extends AbstractApiSubset[Key, HashKey, Value] with RedisRecoverableKeyedApi with RedisBlockingApi {
      type Self[K, H, V] = Blocking[K, H, V]
      protected def copy[K, H, V](newKeyCodec: RedisDataCodec[K], newHashKeyCodec: RedisDataCodec[H], newValueCodec: RedisDataCodec[V]) =
        new Blocking[K, H, V](executor)(newKeyCodec, newHashKeyCodec, newValueCodec, timeout)
    }
    object Blocking {
      case class StringTyped(exec: RedisKeyedExecutor)(implicit timeout: Timeout) extends Blocking[String, String, String](exec)
      case class BinaryTyped(exec: RedisKeyedExecutor)(implicit timeout: Timeout) extends Blocking[ByteString, ByteString, ByteString](exec)
    }
  }

  object Node {
    class Async[Key: RedisDataCodec, HashKey: RedisDataCodec, Value: RedisDataCodec](val executor: RedisNodeExecutor)
      (implicit val timeout: Timeout) extends AbstractApiSubset[Key, HashKey, Value] with RedisRecoverableNodeApi with RedisAsyncApi {
      type Self[K, H, V] = Async[K, H, V]
      protected def copy[K, H, V](newKeyCodec: RedisDataCodec[K], newHashKeyCodec: RedisDataCodec[H], newValueCodec: RedisDataCodec[V]) =
        new Async[K, H, V](executor)(newKeyCodec, newHashKeyCodec, newValueCodec, timeout)
    }
    object Async {
      case class StringTyped(exec: RedisNodeExecutor)(implicit timeout: Timeout) extends Async[String, String, String](exec)
      case class BinaryTyped(exec: RedisNodeExecutor)(implicit timeout: Timeout) extends Async[ByteString, ByteString, ByteString](exec)
    }

    class Blocking[Key: RedisDataCodec, HashKey: RedisDataCodec, Value: RedisDataCodec](val executor: RedisNodeExecutor)
      (implicit val timeout: Timeout) extends AbstractApiSubset[Key, HashKey, Value] with RedisRecoverableNodeApi with RedisBlockingApi {
      type Self[K, H, V] = Blocking[K, H, V]
      protected def copy[K, H, V](newKeyCodec: RedisDataCodec[K], newHashKeyCodec: RedisDataCodec[H], newValueCodec: RedisDataCodec[V]) =
        new Blocking[K, H, V](executor)(newKeyCodec, newHashKeyCodec, newValueCodec, timeout)
    }
    object Blocking {
      case class StringTyped(exec: RedisNodeExecutor)(implicit timeout: Timeout) extends Blocking[String, String, String](exec)
      case class BinaryTyped(exec: RedisNodeExecutor)(implicit timeout: Timeout) extends Blocking[ByteString, ByteString, ByteString](exec)
    }
  }

  object Connection {
    class Async[Key: RedisDataCodec, HashKey: RedisDataCodec, Value: RedisDataCodec](val executor: RedisConnectionExecutor)
      (implicit val timeout: Timeout) extends AbstractApiSubset[Key, HashKey, Value] with RedisRecoverableConnectionApi with RedisAsyncApi {
      type Self[K, H, V] = Async[K, H, V]
      protected def copy[K, H, V](newKeyCodec: RedisDataCodec[K], newHashKeyCodec: RedisDataCodec[H], newValueCodec: RedisDataCodec[V]) =
        new Async[K, H, V](executor)(newKeyCodec, newHashKeyCodec, newValueCodec, timeout)
    }
    object Async {
      case class StringTyped(exec: RedisConnectionExecutor)(implicit timeout: Timeout) extends Async[String, String, String](exec)
      case class BinaryTyped(exec: RedisConnectionExecutor)(implicit timeout: Timeout) extends Async[ByteString, ByteString, ByteString](exec)
    }

    class Blocking[Key: RedisDataCodec, HashKey: RedisDataCodec, Value: RedisDataCodec](val executor: RedisConnectionExecutor)
      (implicit val timeout: Timeout) extends AbstractApiSubset[Key, HashKey, Value] with RedisRecoverableConnectionApi with RedisBlockingApi {
      type Self[K, H, V] = Blocking[K, H, V]
      protected def copy[K, H, V](newKeyCodec: RedisDataCodec[K], newHashKeyCodec: RedisDataCodec[H], newValueCodec: RedisDataCodec[V]) =
        new Blocking[K, H, V](executor)(newKeyCodec, newHashKeyCodec, newValueCodec, timeout)
    }
    object Blocking {
      case class StringTyped(exec: RedisConnectionExecutor)(implicit timeout: Timeout) extends Blocking[String, String, String](exec)
      case class BinaryTyped(exec: RedisConnectionExecutor)(implicit timeout: Timeout) extends Blocking[ByteString, ByteString, ByteString](exec)
    }
  }
}
