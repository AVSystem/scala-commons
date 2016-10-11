package com.avsystem.commons
package redis.commands

import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis._

trait ListsApi extends ApiSubset {
  def lindex(key: Key, index: Long): Result[Opt[Value]] =
    execute(new Lindex(key, index))
  def linsert(key: Key, pivot: Value, value: Value, before: Boolean = false): Result[Opt[Long]] =
    execute(new Linsert(key, before, pivot, value))
  def llen(key: Key): Result[Long] =
    execute(new Llen(key))
  def lpop(key: Key): Result[Opt[Value]] =
    execute(new Lpop(key))
  def lpush(key: Key, values: Value*): Result[Long] =
    execute(new Lpush(key, values))
  def lpushx(key: Key, value: Value): Result[Long] =
    execute(new Lpushx(key, value))
  def lrange(key: Key, start: Long = 0, stop: Long = -1): Result[Seq[Value]] =
    execute(new Lrange(key, start, stop))
  def lrem(key: Key, value: Value, count: RemCount = RemCount.All): Result[Long] =
    execute(new Lrem(key, count, value))
  def lset(key: Key, index: Long, value: Value): Result[Unit] =
    execute(new Lset(key, index, value))
  def ltrim(key: Key, start: Long = 0, stop: Long = -1): Result[Unit] =
    execute(new Ltrim(key, start, stop))
  def rpop(key: Key): Result[Opt[Value]] =
    execute(new Rpop(key))
  def rpoplpush(source: Key, destination: Key): Result[Opt[Value]] =
    execute(new Rpoplpush(source, destination))
  def rpush(key: Key, values: Value*): Result[Long] =
    execute(new Rpush(key, values))
  def rpushx(key: Key, value: Value): Result[Long] =
    execute(new Rpushx(key, value))

  private final class Lindex(key: Key, index: Long) extends RedisOptDataCommand[Value] with NodeCommand {
    val encoded = encoder("LINDEX").key(key).add(index).result
  }

  private final class Linsert(key: Key, before: Boolean, pivot: Value, value: Value)
    extends RedisPositiveLongCommand with NodeCommand {
    val encoded = encoder("LINSERT").key(key).add(if (before) "BEFORE" else "AFTER").data(pivot).data(value).result
  }

  private final class Llen(key: Key) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("LLEN").key(key).result
  }

  private final class Lpop(key: Key) extends RedisOptDataCommand[Value] with NodeCommand {
    val encoded = encoder("LPOP").key(key).result
  }

  private final class Lpush(key: Key, values: Seq[Value]) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("LPUSH").key(key).datas(values).result
  }

  private final class Lpushx(key: Key, value: Value) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("LPUSHX").key(key).data(value).result
  }

  private final class Lrange(key: Key, start: Long, stop: Long)
    extends RedisDataSeqCommand[Value] with NodeCommand {
    val encoded = encoder("LRANGE").key(key).add(start).add(stop).result
  }

  private final class Lrem(key: Key, count: RemCount, value: Value) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("LREM").key(key).add(count.raw).data(value).result
  }

  private final class Lset(key: Key, index: Long, value: Value) extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("LSET").key(key).add(index).data(value).result
  }

  private final class Ltrim(key: Key, start: Long, stop: Long) extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("LTRIM").key(key).add(start).add(stop).result
  }

  private final class Rpop(key: Key) extends RedisOptDataCommand[Value] with NodeCommand {
    val encoded = encoder("RPOP").key(key).result
  }

  private final class Rpoplpush(source: Key, destination: Key) extends RedisOptDataCommand[Value] with NodeCommand {
    val encoded = encoder("RPOPLPUSH").key(source).key(destination).result
  }

  private final class Rpush(key: Key, values: Seq[Value]) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("RPUSH").key(key).datas(values).result
  }

  private final class Rpushx(key: Key, value: Value) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("RPUSHX").key(key).data(value).result
  }
}

class RemCount private(val raw: Long) extends AnyVal
object RemCount {
  def apply(count: Long, fromHead: Boolean): RemCount = {
    require(count > 0, "Count must be positive")
    new RemCount(if (fromHead) count else -count)
  }
  final val All = new RemCount(0)
  def fromHead(count: Long) = RemCount(count, fromHead = true)
  def fromTail(count: Long) = RemCount(count, fromHead = false)
}
