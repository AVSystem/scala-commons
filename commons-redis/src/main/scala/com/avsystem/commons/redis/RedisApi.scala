package com.avsystem.commons
package redis

import akka.util.ByteString
import com.avsystem.commons.redis.config.ExecutionConfig

/**
  * Object which contains implementations of various variants of Redis API that this driver provides.
  * Each variant implements a set of methods corresponding directly to Redis commands, e.g.
  * [[commands.StringsApi.get get]] method represents Redis `GET` command.
  *
  * API variants may differ from each other in several independent aspects:
  *
  * The most important one is the result type returned by every method corresponding to a Redis command:
  *
  *  - [[RedisApi.Raw]] - type returned for each command-method is [[RawCommand]]. Probably the only reason
  * to use this API variant is to pass a [[RawCommand]] to
  * [[commands.NodeServerApi.commandGetkeys(command:com\.avsystem\.commons\.redis\.RawCommand)* commandGetkeys]]
  *  - [[RedisApi.Batches]] - type returned for each command-method is [[RedisBatch]].
  * Batch can then be combined with other batches to form larger batches, become a part of [[RedisOp]] or simply
  * get executed by [[RedisExecutor]] (e.g. one of Redis client implementations).
  *  - [[RedisApi.Keyed.Async]], [[RedisApi.Node.Async]] and [[RedisApi.Connection.Async]] variants are the ones that
  * actually ''execute'' commands by sending them to Redis. Because of that, they take an appropriate [[RedisExecutor]]
  * as constructor argument. Execution of commands is asynchronous and results are returned as
  * `Future`s.
  *  - [[RedisApi.Keyed.Blocking]], [[RedisApi.Node.Blocking]] and [[RedisApi.Connection.Blocking]] are similar
  * to their `Async` counterparts but execution is blocking and results are returned as unwrapped values.
  *
  * `Async` and `Blocking` API variants additionally come in three different "levels", each one exposing different
  * subset of Redis commands. This reflects the fact that not every [[RedisExecutor]] (client implementation) supports
  * every command (e.g. you can't execute unkeyed commands using [[RedisClusterClient]]).
  *
  *  - Variants from [[RedisApi.Keyed]] include only commands with keys (so that they can be executed on Redis Cluster)
  *  - Variants from [[RedisApi.Node]] include only commands which don't access connection state
  *  - Variants from [[RedisApi.Connection]] include all commands supported by the driver
  *
  * Every API variant may also use different types to represent Redis keys, hash keys and values.
  * You can define your own API variants for arbitrary combination of key, hash key and value types
  * as long as there is an instance of [[RedisDataCodec]] for every of these types.
  *
  * For example, if you keep only numeric values on a Redis Cluster installation, you might define an API variant
  * like this one:
  * {{{
  *   class NumericKeyedAsyncApi(executor: RedisKeyedExecutor, config: ExecutionConfig = ExecutionConfig.Default)
  *     extends RedisApi.Keyed.Async[String,String,Long](executor, config)
  * }}}
  *
  * API variants which use only `String`s (textual) or only `ByteString`s (binary) are already implemented by the driver, e.g.
  * [[RedisApi.Keyed.Async.StringTyped]], [[RedisApi.Batches.BinaryTyped]].
  *
  * Note that [[RedisDataCodec]] is automatically provided for many simple types and also all types which have a
  * `GenCodec`. This effectively gives you a complete serialization
  * framework for keys, hash keys and values stored in Redis.
  *
  * Note that chosen key, hash key and value types can be adjusted "on the fly" with a convenient syntax.
  * For example, if you need to use some case class as a value type in a single, specific place, you can do it
  * without defining a completely separate API variant. For example:
  *
  * {{{
  *   case class Person(name: String, birthYear: Int)
  *   object Person {
  *     implicit val codec: GenCodec[Person] = GenCodec.materialize[Person]
  *   }
  *
  *   import scala.concurrent.duration._
  *   implicit val system: ActorSystem = ActorSystem()
  *
  *   val api = RedisApi.Keyed.Blocking.StringTyped(new RedisClusterClient)
  *
  *   // normally, we're just using String-typed API
  *   api.set("key", "value")
  *
  *   // but in one specific place, we might want to use Person as the value
  *   // Person has an instance of GenCodec, so it will be automatically serialized to binary format
  *   api.valueType[Person].set("binaryDataKey", Person("Fred", 1990))
  * }}}
  */
object RedisApi {
  class Raw[Key: RedisDataCodec, Field: RedisDataCodec, Value: RedisDataCodec, Record: RedisRecordCodec]
    extends AbstractRedisApi[Raw, Key, Field, Value, Record] with RedisConnectionApi with RedisRawApi {

    def copy[K, F, V, R](
      kc: RedisDataCodec[K], fc: RedisDataCodec[F], vc: RedisDataCodec[V], rc: RedisRecordCodec[R]
    ): Raw[K, F, V, R] = new Raw[K, F, V, R]()(kc, fc, vc, rc)
  }
  /**
    * Entry point for API variants which return [[RawCommand]]s.
    */
  object Raw {
    object StringTyped extends Raw[String, String, String, Map[String, String]]
    object BinaryTyped extends Raw[ByteString, ByteString, ByteString, Map[ByteString, ByteString]]
  }

  class Batches[Key: RedisDataCodec, Field: RedisDataCodec, Value: RedisDataCodec, Record: RedisRecordCodec]
    extends AbstractRedisApi[Batches, Key, Field, Value, Record] with RedisConnectionApi with RedisBatchApi {

    def copy[K, F, V, R](
      kc: RedisDataCodec[K], fc: RedisDataCodec[F], vc: RedisDataCodec[V], rc: RedisRecordCodec[R]
    ): Batches[K, F, V, R] = new Batches[K, F, V, R]()(kc, fc, vc, rc)
  }
  /**
    * Entry point for API variants which return [[RedisBatch]]es.
    */
  object Batches {
    object StringTyped extends Batches[String, String, String, Map[String, String]]
    object BinaryTyped extends Batches[ByteString, ByteString, ByteString, Map[ByteString, ByteString]]
  }

  /**
    * Entry point for API variants which expose only keyed commands.
    */
  object Keyed {
    class Async[Key: RedisDataCodec, Field: RedisDataCodec, Value: RedisDataCodec, Record: RedisRecordCodec](
      val executor: RedisKeyedExecutor,
      val execConfig: ExecutionConfig = ExecutionConfig.Default
    ) extends AbstractRedisApi[Async, Key, Field, Value, Record] with RedisRecoverableKeyedApi with RedisAsyncApi {
      def copy[K, F, V, R](
        kc: RedisDataCodec[K], fc: RedisDataCodec[F], vc: RedisDataCodec[V], rc: RedisRecordCodec[R]
      ): Async[K, F, V, R] = new Async[K, F, V, R](executor, execConfig)(kc, fc, vc, rc)
    }
    /**
      * Entry point for API variants which execute commands using [[RedisKeyedExecutor]] (e.g. [[RedisClusterClient]])
      * and return results as `Future`s.
      */
    object Async {
      case class StringTyped(exec: RedisKeyedExecutor, config: ExecutionConfig = ExecutionConfig.Default)
        extends Async[String, String, String, Map[String, String]](exec, config)
      case class BinaryTyped(exec: RedisKeyedExecutor, config: ExecutionConfig = ExecutionConfig.Default)
        extends Async[ByteString, ByteString, ByteString, Map[ByteString, ByteString]](exec, config)
    }

    class Blocking[Key: RedisDataCodec, Field: RedisDataCodec, Value: RedisDataCodec, Record: RedisRecordCodec](
      val executor: RedisKeyedExecutor,
      val execConfig: ExecutionConfig = ExecutionConfig.Default
    ) extends AbstractRedisApi[Blocking, Key, Field, Value, Record] with RedisRecoverableKeyedApi with RedisBlockingApi {
      def copy[K, F, V, R](
        kc: RedisDataCodec[K], fc: RedisDataCodec[F], vc: RedisDataCodec[V], rc: RedisRecordCodec[R]
      ): Blocking[K, F, V, R] = new Blocking[K, F, V, R](executor, execConfig)(kc, fc, vc, rc)
    }
    /**
      * Entry point for API variants which execute commands using [[RedisKeyedExecutor]] (e.g. [[RedisClusterClient]])
      * and return results synchronously.
      */
    object Blocking {
      case class StringTyped(exec: RedisKeyedExecutor, config: ExecutionConfig = ExecutionConfig.Default)
        extends Blocking[String, String, String, Map[String, String]](exec, config)
      case class BinaryTyped(exec: RedisKeyedExecutor, config: ExecutionConfig = ExecutionConfig.Default)
        extends Blocking[ByteString, ByteString, ByteString, Map[ByteString, ByteString]](exec, config)
    }
  }

  /**
    * Entry point for API variants which expose node-level commands, i.e. the ones that don't access or modify
    * Redis connection state.
    */
  object Node {
    class Async[Key: RedisDataCodec, Field: RedisDataCodec, Value: RedisDataCodec, Record: RedisRecordCodec](
      val executor: RedisNodeExecutor,
      val execConfig: ExecutionConfig = ExecutionConfig.Default
    ) extends AbstractRedisApi[Async, Key, Field, Value, Record] with RedisRecoverableNodeApi with RedisAsyncApi {
      def copy[K, F, V, R](
        kc: RedisDataCodec[K], fc: RedisDataCodec[F], vc: RedisDataCodec[V], rc: RedisRecordCodec[R]
      ): Async[K, F, V, R] = new Async[K, F, V, R](executor, execConfig)(kc, fc, vc, rc)
    }
    /**
      * Entry point for API variants which execute commands using [[RedisNodeExecutor]] (e.g. [[RedisNodeClient]])
      * and return results as `Future`s.
      */
    object Async {
      case class StringTyped(exec: RedisNodeExecutor, config: ExecutionConfig = ExecutionConfig.Default)
        extends Async[String, String, String, Map[String, String]](exec, config)
      case class BinaryTyped(exec: RedisNodeExecutor, config: ExecutionConfig = ExecutionConfig.Default)
        extends Async[ByteString, ByteString, ByteString, Map[ByteString, ByteString]](exec, config)
    }

    class Blocking[Key: RedisDataCodec, Field: RedisDataCodec, Value: RedisDataCodec, Record: RedisRecordCodec](
      val executor: RedisNodeExecutor,
      val execConfig: ExecutionConfig = ExecutionConfig.Default
    ) extends AbstractRedisApi[Blocking, Key, Field, Value, Record] with RedisRecoverableNodeApi with RedisBlockingApi {
      def copy[K, F, V, R](
        kc: RedisDataCodec[K], fc: RedisDataCodec[F], vc: RedisDataCodec[V], rc: RedisRecordCodec[R]
      ): Blocking[K, F, V, R] = new Blocking[K, F, V, R](executor, execConfig)(kc, fc, vc, rc)
    }
    /**
      * Entry point for API variants which execute commands using [[RedisNodeExecutor]] (e.g. [[RedisNodeClient]])
      * and return results synchronously.
      */
    object Blocking {
      case class StringTyped(exec: RedisNodeExecutor, config: ExecutionConfig = ExecutionConfig.Default)
        extends Blocking[String, String, String, Map[String, String]](exec, config)
      case class BinaryTyped(exec: RedisNodeExecutor, config: ExecutionConfig = ExecutionConfig.Default)
        extends Blocking[ByteString, ByteString, ByteString, Map[ByteString, ByteString]](exec, config)
    }
  }

  /**
    * Entry point for API variants which expose all commands, including connection-level ones, i.e. the ones that
    * access or modify Redis connection state.
    */
  object Connection {
    class Async[Key: RedisDataCodec, Field: RedisDataCodec, Value: RedisDataCodec, Record: RedisRecordCodec](
      val executor: RedisConnectionExecutor,
      val execConfig: ExecutionConfig = ExecutionConfig.Default
    ) extends AbstractRedisApi[Async, Key, Field, Value, Record] with RedisRecoverableConnectionApi with RedisAsyncApi {
      def copy[K, F, V, R](
        kc: RedisDataCodec[K], fc: RedisDataCodec[F], vc: RedisDataCodec[V], rc: RedisRecordCodec[R]
      ): Async[K, F, V, R] = new Async[K, F, V, R](executor, execConfig)(kc, fc, vc, rc)
    }
    /**
      * Entry point for API variants which execute commands using [[RedisConnectionExecutor]] (e.g. [[RedisConnectionClient]])
      * and return results as `Future`s.
      */
    object Async {
      case class StringTyped(exec: RedisConnectionExecutor, config: ExecutionConfig = ExecutionConfig.Default)
        extends Async[String, String, String, Map[String, String]](exec, config)
      case class BinaryTyped(exec: RedisConnectionExecutor, config: ExecutionConfig = ExecutionConfig.Default)
        extends Async[ByteString, ByteString, ByteString, Map[ByteString, ByteString]](exec, config)
    }

    class Blocking[Key: RedisDataCodec, Field: RedisDataCodec, Value: RedisDataCodec, Record: RedisRecordCodec](
      val executor: RedisConnectionExecutor,
      val execConfig: ExecutionConfig = ExecutionConfig.Default
    ) extends AbstractRedisApi[Blocking, Key, Field, Value, Record] with RedisRecoverableConnectionApi with RedisBlockingApi {
      def copy[K, F, V, R](
        kc: RedisDataCodec[K], fc: RedisDataCodec[F], vc: RedisDataCodec[V], rc: RedisRecordCodec[R]
      ): Blocking[K, F, V, R] = new Blocking[K, F, V, R](executor, execConfig)(kc, fc, vc, rc)
    }
    /**
      * Entry point for API variants which execute commands using [[RedisConnectionExecutor]] (e.g. [[RedisConnectionClient]])
      * and return results synchronously.
      */
    object Blocking {
      case class StringTyped(exec: RedisConnectionExecutor, config: ExecutionConfig = ExecutionConfig.Default)
        extends Blocking[String, String, String, Map[String, String]](exec, config)
      case class BinaryTyped(exec: RedisConnectionExecutor, config: ExecutionConfig = ExecutionConfig.Default)
        extends Blocking[ByteString, ByteString, ByteString, Map[ByteString, ByteString]](exec, config)
    }
  }
}

abstract class AbstractRedisApi[Self[K0, F0, V0, R0] <: AbstractRedisApi[Self, K0, F0, V0, R0], K, F, V, R](
  implicit val keyCodec: RedisDataCodec[K],
  val fieldCodec: RedisDataCodec[F],
  val valueCodec: RedisDataCodec[V],
  val recordCodec: RedisRecordCodec[R]
) extends ApiSubset {

  type Key = K
  type Field = F
  type Value = V
  type Record = R

  type WithKey[K0] = Self[K0, F, V, R]
  type WithField[F0] = Self[K, F0, V, R]
  type WithValue[V0] = Self[K, F, V0, R]
  type WithRecord[R0] = Self[K, F, V, R0]

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
  def fieldType[F0: RedisDataCodec]: WithField[F0] =
    copy(newFieldCodec = RedisDataCodec[F0])

  /**
    * Changes the type of hash key used by this API variant to some other type by directly providing
    * functions which convert between new and old type.
    */
  def transformField[F0](read: F => F0)(write: F0 => F): WithField[F0] =
    copy(newFieldCodec = RedisDataCodec(fieldCodec.read andThen read, write andThen fieldCodec.write))

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

  def recordType[R0: RedisRecordCodec]: WithRecord[R0] =
    copy(newRecordCodec = RedisRecordCodec[R0])

  def transformRecord[R0](read: R => R0)(write: R0 => R): WithRecord[R0] =
    copy(newRecordCodec = RedisRecordCodec(recordCodec.read andThen read, write andThen recordCodec.write))

  def copy[K0, F0, V0, R0](
    newKeyCodec: RedisDataCodec[K0] = keyCodec,
    newFieldCodec: RedisDataCodec[F0] = fieldCodec,
    newValueCodec: RedisDataCodec[V0] = valueCodec,
    newRecordCodec: RedisRecordCodec[R0] = recordCodec
  ): Self[K0, F0, V0, R0]
}
