package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis._
import com.avsystem.commons.redis.commands.ReplyDecoders._

trait ListsApi extends ApiSubset {
  /** Executes [[http://redis.io/commands/lindex LINDEX]] */
  def lindex(key: Key, index: Long): Result[Opt[Value]] =
    execute(new Lindex(key, index))
  /** Executes [[http://redis.io/commands/linsert LINSERT]] */
  def linsert(key: Key, pivot: Value, value: Value, before: Boolean = false): Result[Opt[Long]] =
    execute(new Linsert(key, before, pivot, value))
  /** Executes [[http://redis.io/commands/llen LLEN]] */
  def llen(key: Key): Result[Long] =
    execute(new Llen(key))
  /** Executes [[http://redis.io/commands/lpop LPOP]] */
  def lpop(key: Key): Result[Opt[Value]] =
    execute(new Lpop(key))
  /** Executes [[http://redis.io/commands/lpush LPUSH]] */
  def lpush(key: Key, value: Value, values: Value*): Result[Long] =
    execute(new Lpush(key, value +:: values))
  /** Executes [[http://redis.io/commands/lpush LPUSH]]
    * NOTE: `values` MUST NOT be empty - consider using [[lpushOrLlen]] in such case. */
  def lpush(key: Key, values: Iterable[Value]): Result[Long] =
    execute(new Lpush(key, values))
  /** Executes [[http://redis.io/commands/lpush LPUSH]]
    * or [[http://redis.io/commands/llen LLEN]] when `values` is empty */
  def lpushOrLlen(key: Key, values: Iterable[Value]): Result[Long] =
    if (values.nonEmpty) lpush(key, values) else llen(key)
  /** Executes [[http://redis.io/commands/lpushx LPUSHX]] */
  def lpushx(key: Key, value: Value, values: Value*): Result[Long] =
    execute(new Lpushx(key, value +:: values))
  /** Executes [[http://redis.io/commands/lpushx LPUSHX]] */
  def lpushx(key: Key, values: Iterable[Value]): Result[Long] =
    execute(new Lpushx(key, values))
  /** Executes [[http://redis.io/commands/lpush LPUSHX]]
    * or [[http://redis.io/commands/llen LLEN]] when `values` is empty */
  def lpushxOrLlen(key: Key, values: Iterable[Value]): Result[Long] =
    if (values.nonEmpty) lpushx(key, values) else llen(key)
  /** Executes [[http://redis.io/commands/lrange LRANGE]] */
  def lrange(key: Key, start: Long = 0, stop: Long = -1): Result[Seq[Value]] =
    execute(new Lrange(key, start, stop))
  /** Executes [[http://redis.io/commands/lrem LREM]] */
  def lrem(key: Key, value: Value, count: RemCount = RemCount.All): Result[Long] =
    execute(new Lrem(key, count, value))
  /** Executes [[http://redis.io/commands/lset LSET]] */
  def lset(key: Key, index: Long, value: Value): Result[Unit] =
    execute(new Lset(key, index, value))
  /** Executes [[http://redis.io/commands/ltrim LTRIM]] */
  def ltrim(key: Key, start: Long = 0, stop: Long = -1): Result[Unit] =
    execute(new Ltrim(key, start, stop))
  /** Executes [[http://redis.io/commands/rpop RPOP]] */
  def rpop(key: Key): Result[Opt[Value]] =
    execute(new Rpop(key))
  /** Executes [[http://redis.io/commands/rpoplpush RPOPLPUSH]] */
  def rpoplpush(source: Key, destination: Key): Result[Opt[Value]] =
    execute(new Rpoplpush(source, destination))
  /** Executes [[http://redis.io/commands/rpush RPUSH]] */
  def rpush(key: Key, value: Value, values: Value*): Result[Long] =
    execute(new Rpush(key, value +:: values))
  /** Executes [[http://redis.io/commands/rpush RPUSH]]
    * NOTE: `values` MUST NOT be empty - consider using [[rpushOrLlen]] in such case. */
  def rpush(key: Key, values: Iterable[Value]): Result[Long] =
    execute(new Rpush(key, values))
  /** Executes [[http://redis.io/commands/lpush RPUSH]]
    * or [[http://redis.io/commands/llen LLEN]] when `values` is empty */
  def rpushOrLlen(key: Key, values: Iterable[Value]): Result[Long] =
    if (values.nonEmpty) rpush(key, values) else llen(key)
  /** Executes [[http://redis.io/commands/rpushx RPUSHX]] */
  def rpushx(key: Key, value: Value, values: Value*): Result[Long] =
    execute(new Rpushx(key, value +:: values))
  /** Executes [[http://redis.io/commands/rpushx RPUSHX]] */
  def rpushx(key: Key, values: Iterable[Value]): Result[Long] =
    execute(new Rpushx(key, values))
  /** Executes [[http://redis.io/commands/lpush RPUSHX]]
    * or [[http://redis.io/commands/llen LLEN]] when `values` is empty */
  def rpushxOrLlen(key: Key, values: Iterable[Value]): Result[Long] =
    if (values.nonEmpty) rpushx(key, values) else llen(key)

  /** Executes [[http://redis.io/commands/blpop BLPOP]] */
  def blpop(key: Key, timeout: Int): Result[Opt[Value]] =
    execute(new Blpop(key.single, timeout).map(_.map(_._2)))
  /** Executes [[http://redis.io/commands/blpop BLPOP]] */
  def blpop(keys: Iterable[Key], timeout: Int): Result[Opt[(Key, Value)]] =
    execute(new Blpop(keys, timeout))
  /** Executes [[http://redis.io/commands/brpop BRPOP]] */
  def brpop(key: Key, timeout: Int): Result[Opt[Value]] =
    execute(new Brpop(key.single, timeout).map(_.map(_._2)))
  /** Executes [[http://redis.io/commands/brpop BRPOP]] */
  def brpop(keys: Iterable[Key], timeout: Int): Result[Opt[(Key, Value)]] =
    execute(new Brpop(keys, timeout))
  /** Executes [[http://redis.io/commands/brpoplpush BRPOPLPUSH]] */
  def brpoplpush(source: Key, destination: Key, timeout: Int): Result[Opt[Value]] =
    execute(new Brpoplpush(source, destination, timeout))

  private final class Lindex(key: Key, index: Long) extends RedisOptDataCommand[Value] with NodeCommand {
    val encoded: Encoded = encoder("LINDEX").key(key).add(index).result
  }

  private final class Linsert(key: Key, before: Boolean, pivot: Value, value: Value)
    extends RedisPositiveLongCommand with NodeCommand {
    val encoded: Encoded = encoder("LINSERT").key(key).add(if (before) "BEFORE" else "AFTER").data(pivot).data(value).result
  }

  private final class Llen(key: Key) extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("LLEN").key(key).result
  }

  private final class Lpop(key: Key) extends RedisOptDataCommand[Value] with NodeCommand {
    val encoded: Encoded = encoder("LPOP").key(key).result
  }

  private final class Lpush(key: Key, values: Iterable[Value]) extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("LPUSH").key(key).datas(values).result
  }

  private final class Lpushx(key: Key, values: Iterable[Value]) extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("LPUSHX").key(key).datas(values).result
  }

  private final class Lrange(key: Key, start: Long, stop: Long)
    extends RedisDataSeqCommand[Value] with NodeCommand {
    val encoded: Encoded = encoder("LRANGE").key(key).add(start).add(stop).result
  }

  private final class Lrem(key: Key, count: RemCount, value: Value) extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("LREM").key(key).add(count.raw).data(value).result
  }

  private final class Lset(key: Key, index: Long, value: Value) extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("LSET").key(key).add(index).data(value).result
  }

  private final class Ltrim(key: Key, start: Long, stop: Long) extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("LTRIM").key(key).add(start).add(stop).result
  }

  private final class Rpop(key: Key) extends RedisOptDataCommand[Value] with NodeCommand {
    val encoded: Encoded = encoder("RPOP").key(key).result
  }

  private final class Rpoplpush(source: Key, destination: Key) extends RedisOptDataCommand[Value] with NodeCommand {
    val encoded: Encoded = encoder("RPOPLPUSH").key(source).key(destination).result
  }

  private final class Rpush(key: Key, values: Iterable[Value]) extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("RPUSH").key(key).datas(values).result
  }

  private final class Rpushx(key: Key, values: Iterable[Value]) extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("RPUSHX").key(key).datas(values).result
  }

  private final class Blpop(keys: Iterable[Key], timeout: Int)
    extends AbstractRedisCommand[Opt[(Key, Value)]](nullMultiBulkOr(multiBulkPair(bulk[Key], bulk[Value]))) with NodeCommand {
    val encoded: Encoded = encoder("BLPOP").keys(keys).add(timeout).result
    override def maxBlockingMillis: Int =
      if (timeout <= 0) Int.MaxValue else timeout * 1000
  }

  private final class Brpop(keys: Iterable[Key], timeout: Int)
    extends AbstractRedisCommand[Opt[(Key, Value)]](nullMultiBulkOr(multiBulkPair(bulk[Key], bulk[Value]))) with NodeCommand {
    val encoded: Encoded = encoder("BRPOP").keys(keys).add(timeout).result
    override def maxBlockingMillis: Int =
      if (timeout <= 0) Int.MaxValue else timeout * 1000
  }

  private final class Brpoplpush(source: Key, destination: Key, timeout: Int)
    extends AbstractRedisCommand[Opt[Value]](nullMultiBulkOr(bulk[Value])) with NodeCommand {
    val encoded: Encoded = encoder("BRPOPLPUSH").key(source).key(destination).add(timeout).result
    override def maxBlockingMillis: Int =
      if (timeout == 0) Int.MaxValue else timeout * 1000
  }
}

class RemCount private(val raw: Long) extends AnyVal {
  def count: Long = math.abs(raw)
  def fromHead: Boolean = raw > 0
  def fromTail: Boolean = raw < 0
}
object RemCount {
  def apply(count: Long, fromHead: Boolean): RemCount = {
    require(count > 0, "Count must be positive")
    new RemCount(if (fromHead) count else -count)
  }
  final val All = new RemCount(0)
  def fromHead(count: Long) = RemCount(count, fromHead = true)
  def fromTail(count: Long) = RemCount(count, fromHead = false)
}
