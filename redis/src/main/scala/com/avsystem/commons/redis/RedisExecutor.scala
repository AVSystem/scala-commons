package com.avsystem.commons
package redis

import com.avsystem.commons.redis.config.ExecutionConfig

import java.io.Closeable

/**
  * Base trait for Redis clients with ability to execute [[RedisBatch]]es.
  */
trait RedisExecutor {
  implicit def executionContext: ExecutionContext
  /**
    * Executes a [[RedisBatch]]. Redis client implementations (e.g. [[RedisNodeClient]]) implement this method
    * by actually sending the batch to Redis server and receving and decoding the response.
    *
    * WARNING: Even though the signature of this method indicates that any batch can be executed, every client
    * type supports only a subset of commands. For example, you can't execute `CLIENT SETNAME`
    * ([[commands.ConnectionServerApi#clientSetname clientSetname]])
    * on a [[RedisNodeClient]] because it's a connection state changing command and [[RedisNodeClient]] uses a
    * pool of reusable connections. If you try to do this, you'll get a
    * [[exception.ForbiddenCommandException ForbiddenCommandException]].
    */
  def executeBatch[A](batch: RedisBatch[A], config: ExecutionConfig = ExecutionConfig.Default): Future[A]
}

/**
  * Base trait for Redis clients with ability to execute [[RedisOp]]s.
  */
trait RedisOpExecutor {
  /**
    * Executes a [[RedisOp]], i.e. a sequence of [[RedisBatch]]es where each batch may be created based on a result of
    * previous batch and may use `WATCH` and `UNWATCH` commands for the purpose of performing transactions with optimistic
    * locking. Redis client implementations (e.g. [[RedisNodeClient]]) implement execution of [[RedisOp]] by
    * reserving a single connection so that entire [[RedisOp]] is executed on that single connection without any
    * other concurrent commands executing in between. This is necessary for `WATCH` and `UNWATCH` commands to
    * work as expected.
    */
  def executeOp[A](op: RedisOp[A], executionConfig: ExecutionConfig = ExecutionConfig.Default): Future[A]
}

/**
  * Marker subtype of [[RedisExecutor]] which guarantees ability to execute Redis commands which contain keys.
  */
trait RedisKeyedExecutor extends RedisExecutor
/**
  * Marker subtype of [[RedisExecutor]] which guarantees ability to execute commands which do NOT access or change
  * state of a single Redis connection (e.g. `CLIENT SETNAME`).
  */
trait RedisNodeExecutor extends RedisKeyedExecutor with RedisOpExecutor
/**
  * Marker subtype of [[RedisExecutor]] which guarantees ability to execute all commands implemented by
  * the driver, including the ones that change or access connection state.
  */
trait RedisConnectionExecutor extends RedisNodeExecutor

@deprecated("Redis driver is scheduled for removal. It has not been actively tested since v2.21.0. Use a different library, e.g. redisson.", "2.21.0")
abstract class RedisClient extends RedisExecutor with Closeable {
  def initialized: Future[this.type]
}
