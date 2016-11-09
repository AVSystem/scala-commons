package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis._

trait HyperLogLogApi extends ApiSubset {
  /** [[http://redis.io/commands/pfadd PFADD]] */
  def pfadd(key: Key, element: Value, elements: Value*): Result[Boolean] =
    execute(new Pfadd(key, element +:: elements))
  /** [[http://redis.io/commands/pfadd PFADD]] */
  def pfadd(key: Key, elements: Iterable[Value]): Result[Boolean] =
    execute(new Pfadd(key, elements))
  /** [[http://redis.io/commands/pfcount PFCOUNT]] */
  def pfcount(key: Key, keys: Key*): Result[Long] =
    execute(new Pfcount(key +:: keys))
  /** [[http://redis.io/commands/pfcount PFCOUNT]] */
  def pfcount(keys: Iterable[Key]): Result[Long] =
    execute(new Pfcount(keys))
  /** [[http://redis.io/commands/pfmerge PFMERGE]] */
  def pfmerge(destkey: Key, sourcekey: Key, sourcekeys: Key*): Result[Unit] =
    execute(new Pfmerge(destkey, sourcekey +:: sourcekeys))
  /** [[http://redis.io/commands/pfmerge PFMERGE]] */
  def pfmerge(destkey: Key, sourcekeys: Iterable[Key]): Result[Unit] =
    execute(new Pfmerge(destkey, sourcekeys))

  private final class Pfadd(key: Key, elements: Iterable[Value]) extends RedisBooleanCommand with NodeCommand {
    requireNonEmpty(elements, "elements")
    val encoded = encoder("PFADD").key(key).datas(elements).result
  }

  private final class Pfcount(keys: Iterable[Key]) extends RedisLongCommand with NodeCommand {
    requireNonEmpty(keys, "keys")
    val encoded = encoder("PFCOUNT").keys(keys).result
  }

  private final class Pfmerge(destkey: Key, sourcekeys: Iterable[Key]) extends RedisUnitCommand with NodeCommand {
    requireNonEmpty(sourcekeys, "sourcekeys")
    val encoded = encoder("PFMERGE").key(destkey).keys(sourcekeys).result
  }
}
