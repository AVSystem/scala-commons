package com.avsystem.commons
package redis

import com.avsystem.commons.misc.ValueOf
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
  * Also, it is possible to customize `Record` type which is used primarily for entries in Redis Stream API.
  * `Record` type requires [[RedisRecordCodec]] instance.
  *
  * Key, field, value and record types and their serialization typeclass instances are enapsulated by
  * [[RedisSerialization]] instances. API variants are then parameterized with them.
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
  class Raw[S <: RedisSerialization](val serialization: S)
    extends AbstractRedisApi[S] with RedisConnectionApi with RedisRawApi {

    type Self[S0 <: RedisSerialization] = Raw[S0]
    def withSerialization[S0 <: RedisSerialization](ser: S0): Raw[S0] = new Raw(ser)
  }

  /**
    * Entry point for API variants which return [[RawCommand]]s.
    */
  object Raw {
    def apply[S <: RedisSerialization : ValueOf]: Raw[S] = new Raw(ValueOf[S])

    object StringTyped extends Raw[RedisSerialization.Strings.type](RedisSerialization.Strings)
    object BinaryTyped extends Raw[RedisSerialization.ByteStrings.type](RedisSerialization.ByteStrings)
  }

  class Batches[S <: RedisSerialization](val serialization: S)
    extends AbstractRedisApi[S] with RedisConnectionApi with RedisBatchApi {

    type Self[S0 <: RedisSerialization] = Batches[S0]
    def withSerialization[S0 <: RedisSerialization](ser: S0): Batches[S0] = new Batches(ser)
  }

  /**
    * Entry point for API variants which return [[RedisBatch]]es.
    */
  object Batches {
    def apply[S <: RedisSerialization : ValueOf]: Batches[S] = new Batches(ValueOf[S])

    object StringTyped extends Batches[RedisSerialization.Strings.type](RedisSerialization.Strings)
    object BinaryTyped extends Batches[RedisSerialization.ByteStrings.type](RedisSerialization.ByteStrings)
  }

  /**
    * Entry point for API variants which expose only keyed commands.
    */
  object Keyed {
    class Async[S <: RedisSerialization](
      val serialization: S,
      val executor: RedisKeyedExecutor,
      val execConfig: ExecutionConfig = ExecutionConfig.Default
    ) extends AbstractRedisApi[S] with RedisRecoverableKeyedApi with RedisAsyncApi {
      type Self[S0 <: RedisSerialization] = Async[S0]
      def withSerialization[S0 <: RedisSerialization](ser: S0): Async[S0] = new Async(ser, executor, execConfig)
    }

    /**
      * Entry point for API variants which execute commands using [[RedisKeyedExecutor]] (e.g. [[RedisClusterClient]])
      * and return results as `Future`s.
      */
    object Async {
      def apply[S <: RedisSerialization : ValueOf](
        executor: RedisKeyedExecutor,
        execConfig: ExecutionConfig = ExecutionConfig.Default
      ): Async[S] = new Async(ValueOf[S], executor, execConfig)

      case class StringTyped(exec: RedisKeyedExecutor, config: ExecutionConfig = ExecutionConfig.Default)
        extends Async[RedisSerialization.Strings.type](RedisSerialization.Strings, exec, config)
      case class BinaryTyped(exec: RedisKeyedExecutor, config: ExecutionConfig = ExecutionConfig.Default)
        extends Async[RedisSerialization.ByteStrings.type](RedisSerialization.ByteStrings, exec, config)
    }

    class Blocking[S <: RedisSerialization](
      val serialization: S,
      val executor: RedisKeyedExecutor,
      val execConfig: ExecutionConfig = ExecutionConfig.Default
    ) extends AbstractRedisApi[S] with RedisRecoverableKeyedApi with RedisBlockingApi {
      type Self[S0 <: RedisSerialization] = Blocking[S0]
      def withSerialization[S0 <: RedisSerialization](ser: S0): Blocking[S0] = new Blocking(ser, executor, execConfig)
    }

    /**
      * Entry point for API variants which execute commands using [[RedisKeyedExecutor]] (e.g. [[RedisClusterClient]])
      * and return results synchronously.
      */
    object Blocking {
      def apply[S <: RedisSerialization : ValueOf](
        executor: RedisKeyedExecutor,
        execConfig: ExecutionConfig = ExecutionConfig.Default
      ): Blocking[S] = new Blocking(ValueOf[S], executor, execConfig)

      case class StringTyped(exec: RedisKeyedExecutor, config: ExecutionConfig = ExecutionConfig.Default)
        extends Blocking[RedisSerialization.Strings.type](RedisSerialization.Strings, exec, config)
      case class BinaryTyped(exec: RedisKeyedExecutor, config: ExecutionConfig = ExecutionConfig.Default)
        extends Blocking[RedisSerialization.ByteStrings.type](RedisSerialization.ByteStrings, exec, config)
    }
  }

  /**
    * Entry point for API variants which expose node-level commands, i.e. the ones that don't access or modify
    * Redis connection state.
    */
  object Node {
    class Async[S <: RedisSerialization](
      val serialization: S,
      val executor: RedisKeyedExecutor,
      val execConfig: ExecutionConfig = ExecutionConfig.Default
    ) extends AbstractRedisApi[S] with RedisRecoverableNodeApi with RedisAsyncApi {
      type Self[S0 <: RedisSerialization] = Async[S0]
      def withSerialization[S0 <: RedisSerialization](ser: S0): Async[S0] = new Async(ser, executor, execConfig)
    }

    /**
      * Entry point for API variants which execute commands using [[RedisNodeExecutor]] (e.g. [[RedisNodeClient]])
      * and return results as `Future`s.
      */
    object Async {
      def apply[S <: RedisSerialization : ValueOf](
        executor: RedisKeyedExecutor,
        execConfig: ExecutionConfig = ExecutionConfig.Default
      ): Async[S] = new Async(ValueOf[S], executor, execConfig)

      case class StringTyped(exec: RedisKeyedExecutor, config: ExecutionConfig = ExecutionConfig.Default)
        extends Async[RedisSerialization.Strings.type](RedisSerialization.Strings, exec, config)
      case class BinaryTyped(exec: RedisKeyedExecutor, config: ExecutionConfig = ExecutionConfig.Default)
        extends Async[RedisSerialization.ByteStrings.type](RedisSerialization.ByteStrings, exec, config)
    }

    class Blocking[S <: RedisSerialization](
      val serialization: S,
      val executor: RedisKeyedExecutor,
      val execConfig: ExecutionConfig = ExecutionConfig.Default
    ) extends AbstractRedisApi[S] with RedisRecoverableNodeApi with RedisBlockingApi {
      type Self[S0 <: RedisSerialization] = Blocking[S0]
      def withSerialization[S0 <: RedisSerialization](ser: S0): Blocking[S0] = new Blocking(ser, executor, execConfig)
    }

    /**
      * Entry point for API variants which execute commands using [[RedisNodeExecutor]] (e.g. [[RedisNodeClient]])
      * and return results synchronously.
      */
    object Blocking {
      def apply[S <: RedisSerialization : ValueOf](
        executor: RedisKeyedExecutor,
        execConfig: ExecutionConfig = ExecutionConfig.Default
      ): Blocking[S] = new Blocking(ValueOf[S], executor, execConfig)

      case class StringTyped(exec: RedisKeyedExecutor, config: ExecutionConfig = ExecutionConfig.Default)
        extends Blocking[RedisSerialization.Strings.type](RedisSerialization.Strings, exec, config)
      case class BinaryTyped(exec: RedisKeyedExecutor, config: ExecutionConfig = ExecutionConfig.Default)
        extends Blocking[RedisSerialization.ByteStrings.type](RedisSerialization.ByteStrings, exec, config)
    }
  }

  /**
    * Entry point for API variants which expose all commands, including connection-level ones, i.e. the ones that
    * access or modify Redis connection state.
    */
  object Connection {
    class Async[S <: RedisSerialization](
      val serialization: S,
      val executor: RedisKeyedExecutor,
      val execConfig: ExecutionConfig = ExecutionConfig.Default
    ) extends AbstractRedisApi[S] with RedisRecoverableConnectionApi with RedisAsyncApi {
      type Self[S0 <: RedisSerialization] = Async[S0]
      def withSerialization[S0 <: RedisSerialization](ser: S0): Async[S0] = new Async(ser, executor, execConfig)
    }

    /**
      * Entry point for API variants which execute commands using [[RedisConnectionExecutor]] (e.g. [[RedisConnectionClient]])
      * and return results as `Future`s.
      */
    object Async {
      def apply[S <: RedisSerialization : ValueOf](
        executor: RedisKeyedExecutor,
        execConfig: ExecutionConfig = ExecutionConfig.Default
      ): Async[S] = new Async(ValueOf[S], executor, execConfig)

      case class StringTyped(exec: RedisKeyedExecutor, config: ExecutionConfig = ExecutionConfig.Default)
        extends Async[RedisSerialization.Strings.type](RedisSerialization.Strings, exec, config)
      case class BinaryTyped(exec: RedisKeyedExecutor, config: ExecutionConfig = ExecutionConfig.Default)
        extends Async[RedisSerialization.ByteStrings.type](RedisSerialization.ByteStrings, exec, config)
    }

    class Blocking[S <: RedisSerialization](
      val serialization: S,
      val executor: RedisKeyedExecutor,
      val execConfig: ExecutionConfig = ExecutionConfig.Default
    ) extends AbstractRedisApi[S] with RedisRecoverableConnectionApi with RedisBlockingApi {
      type Self[S0 <: RedisSerialization] = Blocking[S0]
      def withSerialization[S0 <: RedisSerialization](ser: S0): Blocking[S0] = new Blocking(ser, executor, execConfig)
    }

    /**
      * Entry point for API variants which execute commands using [[RedisConnectionExecutor]]
      * (e.g. [[RedisConnectionClient]]) and return results synchronously.
      */
    object Blocking {
      def apply[S <: RedisSerialization : ValueOf](
        executor: RedisKeyedExecutor,
        execConfig: ExecutionConfig = ExecutionConfig.Default
      ): Blocking[S] = new Blocking(ValueOf[S], executor, execConfig)

      case class StringTyped(exec: RedisKeyedExecutor, config: ExecutionConfig = ExecutionConfig.Default)
        extends Blocking[RedisSerialization.Strings.type](RedisSerialization.Strings, exec, config)
      case class BinaryTyped(exec: RedisKeyedExecutor, config: ExecutionConfig = ExecutionConfig.Default)
        extends Blocking[RedisSerialization.ByteStrings.type](RedisSerialization.ByteStrings, exec, config)
    }
  }
}

abstract class AbstractRedisApi[S <: RedisSerialization] extends ApiSubset {
  type Self[S0 <: RedisSerialization] <: AbstractRedisApi[S0]
  def withSerialization[S0 <: RedisSerialization](ser: S0): Self[S0]

  type WithKey[K] = Self[serialization.WithKey[K]]
  type WithField[F] = Self[serialization.WithField[F]]
  type WithValue[V] = Self[serialization.WithValue[V]]
  type WithRecord[R] = Self[serialization.WithRecord[R]]

  final def keyType[K: RedisDataCodec]: WithKey[K] =
    withSerialization(serialization.keyType[K])

  final def fieldType[F: RedisDataCodec]: WithField[F] =
    withSerialization(serialization.fieldType[F])

  final def valueType[V: RedisDataCodec]: WithValue[V] =
    withSerialization(serialization.valueType[V])

  final def recordType[R: RedisRecordCodec]: WithRecord[R] =
    withSerialization(serialization.recordType[R])
}
