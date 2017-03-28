package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis._

trait HyperLogLogApi extends ApiSubset {
  /** Executes [[http://redis.io/commands/pfadd PFADD]] */
  def pfadd(key: Key, elements: Value*): Result[Boolean] =
    execute(new Pfadd(key, elements))
  /** Executes [[http://redis.io/commands/pfadd PFADD]]
    * NOTE: `elements` CAN be empty, Redis accepts it */
  def pfadd(key: Key, elements: Iterable[Value]): Result[Boolean] =
    execute(new Pfadd(key, elements))
  /** Executes [[http://redis.io/commands/pfcount PFCOUNT]] */
  def pfcount(key: Key, keys: Key*): Result[Long] =
    execute(new Pfcount(key +:: keys))
  /** Executes [[http://redis.io/commands/pfcount PFCOUNT]]
    * or simply returns 0 when `keys` is empty, without sending the command to Redis */
  def pfcount(keys: Iterable[Key]): Result[Long] =
    execute(new Pfcount(keys))
  /** Executes [[http://redis.io/commands/pfmerge PFMERGE]] */
  def pfmerge(destkey: Key, sourcekeys: Key*): Result[Unit] =
    execute(new Pfmerge(destkey, sourcekeys))
  /** Executes [[http://redis.io/commands/pfmerge PFMERGE]].
    * NOTE: `sourcekeys` CAN be empty, Redis accepts it */
  def pfmerge(destkey: Key, sourcekeys: Iterable[Key]): Result[Unit] =
    execute(new Pfmerge(destkey, sourcekeys))

  private final class Pfadd(key: Key, elements: Iterable[Value]) extends RedisBooleanCommand with NodeCommand {
    val encoded = encoder("PFADD").key(key).datas(elements).result
  }

  private final class Pfcount(keys: Iterable[Key]) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("PFCOUNT").keys(keys).result
    override def immediateResult = whenEmpty(keys, 0)
  }

  private final class Pfmerge(destkey: Key, sourcekeys: Iterable[Key]) extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("PFMERGE").key(destkey).keys(sourcekeys).result
  }
}
