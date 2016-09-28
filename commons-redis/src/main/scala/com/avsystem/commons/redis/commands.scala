package com.avsystem.commons
package redis

import akka.util.ByteString
import com.avsystem.commons.redis.commands._
import com.avsystem.commons.redis.util.ByteStringCodec
import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

trait RedisExecutor {
  def execute[A](batch: RedisBatch[A]): Future[A]
}

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

/*
 * These three subtraits provide rudimentary typesafety layer.
 * Their sole purpose is so that e.g. `RedisNodeClient.toExecutor` cannot be passed into `RedisNodeAsyncCommands`.
 * However, it's still easy to circumvent this protection by using client's `executeBatch` method directly or
 * by implementing `RedisConnectionExecutor` manually.
 */
trait RedisClusteredExecutor extends RedisExecutor
trait RedisNodeExecutor extends RedisClusteredExecutor
trait RedisConnectionExecutor extends RedisNodeExecutor

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

  def hashKeyType[H: RedisDataCodec]: WithHashKey[H] =
    copy(newHashKeyCodec = RedisDataCodec[H])

  def valueType[V: RedisDataCodec]: WithValue[V] =
    copy(newValueCodec = RedisDataCodec[V])

  protected implicit def keyCodec: RedisDataCodec[Key]
  protected implicit def hashKeyCodec: RedisDataCodec[HashKey]
  protected implicit def valueCodec: RedisDataCodec[Value]

  protected def copy[K, H, V](
    newKeyCodec: RedisDataCodec[K] = keyCodec,
    newHashKeyCodec: RedisDataCodec[H] = hashKeyCodec,
    newValueCodec: RedisDataCodec[V] = valueCodec
  ): Self[K, H, V]

  protected def execute[A](cmd: RedisCommand[A]): Result[A]

  trait HasKeyCodec extends HasCodec[Key] {
    protected def codec = keyCodec
  }
  trait HasHashKeyCodec extends HasCodec[HashKey] {
    protected def codec = hashKeyCodec
  }
  trait HasValueCodec extends HasCodec[Value] {
    protected def codec = valueCodec
  }
}

abstract class AbstractApiSubset[K, H, V](implicit
  val keyCodec: RedisDataCodec[K], val hashKeyCodec: RedisDataCodec[H], val valueCodec: RedisDataCodec[V])
  extends ApiSubset {
  type Key = K
  type HashKey = H
  type Value = V
}

trait CommandSubset extends ApiSubset {
  type Result[A] = RedisBatch[A]
  type Self[K, H, V] <: AbstractApiSubset[K, H, V] with CommandSubset
  protected def execute[A](cmd: RedisCommand[A]) = cmd
}
trait AsyncCommandSubset extends ApiSubset {
  type Result[A] = Future[A]
  type Self[K, H, V] <: AbstractApiSubset[K, H, V] with AsyncCommandSubset
  protected def executor: RedisClusteredExecutor
  protected def execute[A](cmd: RedisCommand[A]) = executor.execute(cmd)
}
trait BlockingCommandSubset extends ApiSubset {
  type Result[A] = A
  type Self[K, H, V] <: AbstractApiSubset[K, H, V] with BlockingCommandSubset
  protected def timeout: Duration
  protected def executor: RedisClusteredExecutor
  protected def execute[A](cmd: RedisCommand[A]) = Await.result(executor.execute(cmd), timeout)
}

trait RedisClusteredApi extends AnyRef
  with ClusteredKeysApi
  with StringsApi
  with ClusteredServerApi
  with ClusteredClusterApi
  with GeoApi {
  type Self[K, H, V] <: AbstractApiSubset[K, H, V] with RedisClusteredApi
}

trait RedisNodeApi extends RedisClusteredApi
  with NodeKeysApi
  with NodeServerApi
  with NodeClusterApi
  with NodeConnectionApi {
  type Self[K, H, V] <: AbstractApiSubset[K, H, V] with RedisNodeApi
}

trait RedisOperationApi extends RedisNodeApi
  with TransactionApi {
  type Self[K, H, V] <: AbstractApiSubset[K, H, V] with RedisOperationApi
}

trait RedisConnectionApi extends RedisOperationApi
  with ConnectionClusterApi
  with ConnectionConnectionApi {
  type Self[K, H, V] <: AbstractApiSubset[K, H, V] with RedisConnectionApi
}

class RedisCommands[Key: RedisDataCodec, HashKey: RedisDataCodec, Value: RedisDataCodec]
  extends AbstractApiSubset[Key, HashKey, Value] with RedisConnectionApi with CommandSubset {
  type Self[K, H, V] = RedisCommands[K, H, V]
  protected def copy[K, H, V](newKeyCodec: RedisDataCodec[K], newHashKeyCodec: RedisDataCodec[H], newValueCodec: RedisDataCodec[V]) =
    new RedisCommands[K, H, V]()(newKeyCodec, newHashKeyCodec, newValueCodec)
}

object RedisBinaryCommands extends RedisCommands[ByteString, ByteString, ByteString]
object RedisStringCommands extends RedisCommands[String, String, String]

// clustered, async

class RedisClusteredAsyncCommands[Key: RedisDataCodec, HashKey: RedisDataCodec, Value: RedisDataCodec](val executor: RedisClusteredExecutor)
  extends AbstractApiSubset[Key, HashKey, Value] with RedisClusteredApi with AsyncCommandSubset {
  type Self[K, H, V] = RedisClusteredAsyncCommands[K, H, V]
  protected def copy[K, H, V](newKeyCodec: RedisDataCodec[K], newHashKeyCodec: RedisDataCodec[H], newValueCodec: RedisDataCodec[V]) =
    new RedisClusteredAsyncCommands[K, H, V](executor)(newKeyCodec, newHashKeyCodec, newValueCodec)
}

final class RedisBinaryClusteredAsyncCommands(executor: RedisClusteredExecutor)
  extends RedisClusteredAsyncCommands[ByteString, ByteString, ByteString](executor)
final class RedisStringClusteredAsyncCommands(executor: RedisClusteredExecutor)
  extends RedisClusteredAsyncCommands[String, String, String](executor)

// node, async

class RedisNodeAsyncCommands[Key: RedisDataCodec, HashKey: RedisDataCodec, Value: RedisDataCodec](val executor: RedisNodeExecutor)
  extends AbstractApiSubset[Key, HashKey, Value] with RedisNodeApi with AsyncCommandSubset {
  type Self[K, H, V] = RedisNodeAsyncCommands[K, H, V]
  protected def copy[K, H, V](newKeyCodec: RedisDataCodec[K], newHashKeyCodec: RedisDataCodec[H], newValueCodec: RedisDataCodec[V]) =
    new RedisNodeAsyncCommands[K, H, V](executor)(newKeyCodec, newHashKeyCodec, newValueCodec)
}

final class RedisBinaryNodeAsyncCommands(executor: RedisNodeExecutor)
  extends RedisNodeAsyncCommands[ByteString, ByteString, ByteString](executor)
final class RedisStringNodeAsyncCommands(executor: RedisNodeExecutor)
  extends RedisNodeAsyncCommands[String, String, String](executor)

// connection, async

class RedisConnectionAsyncCommands[Key: RedisDataCodec, HashKey: RedisDataCodec, Value: RedisDataCodec](val executor: RedisConnectionExecutor)
  extends AbstractApiSubset[Key, HashKey, Value] with RedisConnectionApi with AsyncCommandSubset {
  type Self[K, H, V] = RedisConnectionAsyncCommands[K, H, V]
  protected def copy[K, H, V](newKeyCodec: RedisDataCodec[K], newHashKeyCodec: RedisDataCodec[H], newValueCodec: RedisDataCodec[V]) =
    new RedisConnectionAsyncCommands[K, H, V](executor)(newKeyCodec, newHashKeyCodec, newValueCodec)
}

final class RedisBinaryConnectionAsyncCommands(executor: RedisConnectionExecutor)
  extends RedisConnectionAsyncCommands[ByteString, ByteString, ByteString](executor)
final class RedisStringConnectionAsyncCommands(executor: RedisConnectionExecutor)
  extends RedisConnectionAsyncCommands[String, String, String](executor)

// clustered, blocking

class RedisClusteredBlockingCommands[Key: RedisDataCodec, HashKey: RedisDataCodec, Value: RedisDataCodec](val executor: RedisClusteredExecutor, val timeout: Duration)
  extends AbstractApiSubset[Key, HashKey, Value] with RedisClusteredApi with BlockingCommandSubset {
  type Self[K, H, V] = RedisClusteredBlockingCommands[K, H, V]
  protected def copy[K, H, V](newKeyCodec: RedisDataCodec[K], newHashKeyCodec: RedisDataCodec[H], newValueCodec: RedisDataCodec[V]) =
    new RedisClusteredBlockingCommands[K, H, V](executor, timeout)(newKeyCodec, newHashKeyCodec, newValueCodec)
}

final class RedisBinaryClusteredBlockingCommands(executor: RedisClusteredExecutor, timeout: Duration)
  extends RedisClusteredBlockingCommands[ByteString, ByteString, ByteString](executor, timeout)
final class RedisStringClusteredBlockingCommands(executor: RedisClusteredExecutor, timeout: Duration)
  extends RedisClusteredBlockingCommands[String, String, String](executor, timeout)

// node, blocking

class RedisNodeBlockingCommands[Key: RedisDataCodec, HashKey: RedisDataCodec, Value: RedisDataCodec](val executor: RedisNodeExecutor, val timeout: Duration)
  extends AbstractApiSubset[Key, HashKey, Value] with RedisNodeApi with BlockingCommandSubset {
  type Self[K, H, V] = RedisNodeBlockingCommands[K, H, V]
  protected def copy[K, H, V](newKeyCodec: RedisDataCodec[K], newHashKeyCodec: RedisDataCodec[H], newValueCodec: RedisDataCodec[V]) =
    new RedisNodeBlockingCommands[K, H, V](executor, timeout)(newKeyCodec, newHashKeyCodec, newValueCodec)
}

final class RedisBinaryNodeBlockingCommands(executor: RedisNodeExecutor, timeout: Duration)
  extends RedisNodeBlockingCommands[ByteString, ByteString, ByteString](executor, timeout)
final class RedisStringNodeBlockingCommands(executor: RedisNodeExecutor, timeout: Duration)
  extends RedisNodeBlockingCommands[String, String, String](executor, timeout)

// connection, blocking

class RedisConnectionBlockingCommands[Key: RedisDataCodec, HashKey: RedisDataCodec, Value: RedisDataCodec](val executor: RedisConnectionExecutor, val timeout: Duration)
  extends AbstractApiSubset[Key, HashKey, Value] with RedisConnectionApi with BlockingCommandSubset {
  type Self[K, H, V] = RedisConnectionBlockingCommands[K, H, V]
  protected def copy[K, H, V](newKeyCodec: RedisDataCodec[K], newHashKeyCodec: RedisDataCodec[H], newValueCodec: RedisDataCodec[V]) =
    new RedisConnectionBlockingCommands[K, H, V](executor, timeout)(newKeyCodec, newHashKeyCodec, newValueCodec)
}

final class RedisBinaryConnectionBlockingCommands(executor: RedisConnectionExecutor, timeout: Duration)
  extends RedisConnectionBlockingCommands[ByteString, ByteString, ByteString](executor, timeout)
final class RedisStringConnectionBlockingCommands(executor: RedisConnectionExecutor, timeout: Duration)
  extends RedisConnectionBlockingCommands[String, String, String](executor, timeout)
