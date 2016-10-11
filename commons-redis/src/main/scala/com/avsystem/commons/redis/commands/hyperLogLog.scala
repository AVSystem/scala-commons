package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis._

trait HyperLogLogApi extends ApiSubset {
  def pfadd(key: Key, elements: Value*): Result[Boolean] =
    execute(new Pfadd(key, elements))
  def pfcount(keys: Key*): Result[Long] =
    execute(new Pfcount(keys))
  def pfmerge(destkey: Key, sourcekeys: Key*): Result[Unit] =
    execute(new Pfmerge(destkey, sourcekeys))

  private final class Pfadd(key: Key, elements: Seq[Value]) extends RedisBooleanCommand with NodeCommand {
    val encoded = encoder("PFADD").key(key).datas(elements).result
  }

  private final class Pfcount(keys: Seq[Key]) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("PFCOUNT").keys(keys).result
  }

  private final class Pfmerge(destkey: Key, sourcekeys: Seq[Key]) extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("PFMERGE").key(destkey).keys(sourcekeys).result
  }
}
