package com.avsystem.commons
package redis

import akka.util.{ByteString, Timeout}

/**
  * Object which contains implementations of various variants of Redis API that this driver provides.
  * Each variant implements a set of methods corresponding directly to Redis commands, e.g.
  * [[commands.StringsApi.get get]] method represents Redis `GET` command.
  *
  * The difference between variants might be any of:
  *
  * <ul>
  * <li>Type returned by each command-method:</li>
  * <ul>
  * <li>[[RedisApi.Raw]] - type returned for each command-method is [[RawCommand]]. Probably the only reason
  * to use this API variant is to pass a [[RawCommand]] to
  * [[commands.NodeServerApi.commandGetkeys(command:com\.avsystem\.commons\.redis\.RawCommand)*]]</li>
  * <li>[[RedisApi.Batches]] - type returned for each command-method is [[RedisBatch]].
  * Batch can then be combined with other batches to form larger batches, become a part of [[RedisOp]] or simply
  * get executed by [[RedisExecutor]] (e.g. one of Redis client implementations).</li>
  * <li>[[RedisApi.Keyed.Async]], [[RedisApi.Node.Async]] and [[RedisApi.Connection.Async]] type returned for each
  * command-method is [[scala.concurrent.Future Future]], i.e. commands are actually executed (asynchronously).</li>
  * <li>[[RedisApi.Keyed.Blocking]], [[RedisApi.Node.Blocking]] and [[RedisApi.Connection.Blocking]] type returned for each
  * command-method is simply the result type of each command, i.e. commands are actually executed (synchronously).</li>
  * </ul>
  * <li>The subset of API supported. This only applies to "executing" variants, i.e. `Async` and `Blocking`.
  * [[RedisApi.Raw]] and [[RedisApi.Batches]] support all commands.</li>
  * <ul>
  * <li>Variants from [[RedisApi.Keyed]] include only commands with keys (so that they can be executed on Redis Cluster)</li>
  * <li>Variants from [[RedisApi.Node]] include only commands which don't access connection state</li>
  * <li>Variants from [[RedisApi.Connection]] include all commands supported by the driver</li>
  * </ul>
  * <li>Types of keys, hash keys and values used in command-methods. You can define your own API variants for
  * arbitrary combination of types chosen for keys, hash keys and values, as long as there is an instance of
  * [[RedisDataCodec]] for every of these types. For example, if you keep only numeric values on a Redis Cluster installation,
  * you might define an API variant like this one:
  * {{{
  *   class NumericKeyedAsyncApi(executor: RedisKeyedExecutor)(implicit timeout: Timeout)
  *     extends RedisApi.Keyed.Async[String,String,Long](executor)
  * }}}
  * By default, the driver always implements variants where all the types are `ByteString`s or `String`s, e.g.
  * [[RedisApi.Keyed.Async.StringTyped]], [[RedisApi.Batches.BinaryTyped]].
  *
  * Note that chosen key, hash key and value types can be adjusted "on the fly" with a convenient syntax.
  * For example, if you need your value to be binary in a single, specific callsite, you can do this:
  *
  * {{{
  *   import scala.concurrent.duration._
  *   implicit val system: ActorSystem = ActorSystem()
  *   implicit val timeout: Timeout = 10.seconds
  *
  *   val api = RedisApi.Keyed.Blocking.StringTyped(new RedisClusterClient)
  *
  *   // normally, we're just using String-typed API
  *   api.set("key", "value")
  *   // but in one specific place, we might need to use binary data as a value
  *   api.valueType[ByteString].set("binaryDataKey", ByteString("binaryData"))
  * }}}
  * </li>
  * </ul>
  */
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

abstract class AbstractRedisApi[Self[K0, H0, V0] <: AbstractRedisApi[Self, K0, H0, V0], K, H, V](implicit
  val keyCodec: RedisDataCodec[K], val hashKeyCodec: RedisDataCodec[H], val valueCodec: RedisDataCodec[V])
  extends ApiSubset {

  type Key = K
  type HashKey = H
  type Value = V

  type WithKey[K0] = Self[K0, H, V]
  type WithHashKey[H0] = Self[K, H0, V]
  type WithValue[V0] = Self[K, H, V0]

  /**
    * Changes the type of key used by this API variant to some other type for which an instance of
    * [[RedisDataCodec]] exists.
    */
  def keyType[K0: RedisDataCodec]: WithKey[K0] =
    copy(newKeyCodec = RedisDataCodec[K0])

  /**
    * Changes the type of key used by this API variant to some other type by directly providing
    * functions which convert between new and old type.
    */
  def transformKey[K0](read: K => K0)(write: K0 => K): WithKey[K0] =
    copy(newKeyCodec = RedisDataCodec(keyCodec.read andThen read, write andThen keyCodec.write))

  /**
    * Changes the type of hash key used by this API variant to some other type for which an instance of
    * [[RedisDataCodec]] exists.
    */
  def hashKeyType[H0: RedisDataCodec]: WithHashKey[H0] =
    copy(newHashKeyCodec = RedisDataCodec[H0])

  /**
    * Changes the type of hash key used by this API variant to some other type by directly providing
    * functions which convert between new and old type.
    */
  def transformHashKey[H0](read: H => H0)(write: H0 => H): WithHashKey[H0] =
    copy(newHashKeyCodec = RedisDataCodec(hashKeyCodec.read andThen read, write andThen hashKeyCodec.write))

  /**
    * Changes the type of value used by this API variant to some other type for which an instance of
    * [[RedisDataCodec]] exists.
    */
  def valueType[V0: RedisDataCodec]: WithValue[V0] =
    copy(newValueCodec = RedisDataCodec[V0])

  /**
    * Changes the type of value used by this API variant to some other type by directly providing
    * functions which convert between new and old type.
    */
  def transformValue[V0](read: V => V0)(write: V0 => V): WithValue[V0] =
    copy(newValueCodec = RedisDataCodec(valueCodec.read andThen read, write andThen valueCodec.write))

  def copy[K0, H0, V0](
    newKeyCodec: RedisDataCodec[K0] = keyCodec,
    newHashKeyCodec: RedisDataCodec[H0] = hashKeyCodec,
    newValueCodec: RedisDataCodec[V0] = valueCodec
  ): Self[K0, H0, V0]
}
