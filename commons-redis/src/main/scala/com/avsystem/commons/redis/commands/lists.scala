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
    if(values.nonEmpty) lpush(key, values) else llen(key)
  /** Executes [[http://redis.io/commands/lpushx LPUSHX]] */
  def lpushx(key: Key, value: Value, values: Value*): Result[Long] =
    execute(new Lpushx(key, value +:: values))
  /** Executes [[http://redis.io/commands/lpushx LPUSHX]] */
  def lpushx(key: Key, values: Iterable[Value]): Result[Long] =
    execute(new Lpushx(key, values))
  /** Executes [[http://redis.io/commands/lpush LPUSHX]]
    * or [[http://redis.io/commands/llen LLEN]] when `values` is empty */
  def lpushxOrLlen(key: Key, values: Iterable[Value]): Result[Long] =
    if(values.nonEmpty) lpushx(key, values) else llen(key)
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
    if(values.nonEmpty) rpush(key, values) else llen(key)
  /** Executes [[http://redis.io/commands/rpushx RPUSHX]] */
  def rpushx(key: Key, value: Value, values: Value*): Result[Long] =
    execute(new Rpushx(key, value +:: values))
  /** Executes [[http://redis.io/commands/rpushx RPUSHX]] */
  def rpushx(key: Key, values: Iterable[Value]): Result[Long] =
    execute(new Rpushx(key, values))
  /** Executes [[http://redis.io/commands/lpush RPUSHX]]
    * or [[http://redis.io/commands/llen LLEN]] when `values` is empty */
  def rpushxOrLlen(key: Key, values: Iterable[Value]): Result[Long] =
    if(values.nonEmpty) rpushx(key, values) else llen(key)

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

  private final class Lpush(key: Key, values: Iterable[Value]) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("LPUSH").key(key).datas(values).result
  }

  private final class Lpushx(key: Key, values: Iterable[Value]) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("LPUSHX").key(key).datas(values).result
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

  private final class Rpush(key: Key, values: Iterable[Value]) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("RPUSH").key(key).datas(values).result
  }

  private final class Rpushx(key: Key, values: Iterable[Value]) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("RPUSHX").key(key).datas(values).result
  }
}

trait BlockingListsApi extends ApiSubset {
  /** Executes [[http://redis.io/commands/blpop BLPOP]] */
  def blpop(key: Key): Result[Value] =
    execute(new Blpop(key.single, 0).map(_.get._2))
  /** Executes [[http://redis.io/commands/blpop BLPOP]] */
  def blpop(key: Key, keys: Key*): Result[(Key, Value)] =
    execute(new Blpop(key +:: keys, 0).map(_.get))
  /** Executes [[http://redis.io/commands/blpop BLPOP]] */
  def blpop(keys: Iterable[Key]): Result[(Key, Value)] =
    execute(new Blpop(keys, 0).map(_.get))
  /** Executes [[http://redis.io/commands/blpop BLPOP]] */
  def blpop(key: Key, timeout: Int): Result[Opt[Value]] =
    execute(new Blpop(key.single, timeout).map(_.map(_._2)))
  /** Executes [[http://redis.io/commands/blpop BLPOP]] */
  def blpop(keys: Iterable[Key], timeout: Int): Result[Opt[(Key, Value)]] =
    execute(new Blpop(keys, timeout))
  /** Executes [[http://redis.io/commands/brpop BRPOP]] */
  def brpop(key: Key): Result[Value] =
    execute(new Brpop(key.single, 0).map(_.get._2))
  /** Executes [[http://redis.io/commands/brpop BRPOP]] */
  def brpop(key: Key, keys: Key*): Result[(Key, Value)] =
    execute(new Brpop(key +:: keys, 0).map(_.get))
  /** Executes [[http://redis.io/commands/brpop BRPOP]] */
  def brpop(keys: Iterable[Key]): Result[(Key, Value)] =
    execute(new Brpop(keys, 0).map(_.get))
  /** Executes [[http://redis.io/commands/brpop BRPOP]] */
  def brpop(key: Key, timeout: Int): Result[Opt[Value]] =
    execute(new Brpop(key.single, timeout).map(_.map(_._2)))
  /** Executes [[http://redis.io/commands/brpop BRPOP]] */
  def brpop(keys: Iterable[Key], timeout: Int): Result[Opt[(Key, Value)]] =
    execute(new Brpop(keys, timeout))
  /** Executes [[http://redis.io/commands/brpoplpush BRPOPLPUSH]] */
  def brpoplpush(source: Key, destination: Key): Result[Value] =
    execute(new Brpoplpush(source, destination, 0).map(_.get))
  /** Executes [[http://redis.io/commands/brpoplpush BRPOPLPUSH]] */
  def brpoplpush(source: Key, destination: Key, timeout: Int): Result[Opt[Value]] =
    execute(new Brpoplpush(source, destination, timeout))

  private final class Blpop(keys: Iterable[Key], timeout: Int)
    extends AbstractRedisCommand[Opt[(Key, Value)]](nullMultiBulkOr(multiBulkPair(bulk[Key], bulk[Value]))) with ConnectionCommand {
    val encoded = encoder("BLPOP").keys(keys).add(timeout).result
  }

  private final class Brpop(keys: Iterable[Key], timeout: Int)
    extends AbstractRedisCommand[Opt[(Key, Value)]](nullMultiBulkOr(multiBulkPair(bulk[Key], bulk[Value]))) with ConnectionCommand {
    val encoded = encoder("BRPOP").keys(keys).add(timeout).result
  }

  private final class Brpoplpush(source: Key, destination: Key, timeout: Int)
    extends AbstractRedisCommand[Opt[Value]](nullMultiBulkOr(bulk[Value])) with ConnectionCommand {
    val encoded = encoder("BRPOPLPUSH").key(source).key(destination).add(timeout).result
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
