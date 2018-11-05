package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis._
import com.avsystem.commons.redis.commands.ReplyDecoders._

trait SetsApi extends ApiSubset {
  /** Executes [[http://redis.io/commands/sadd SADD]] */
  def sadd(key: Key, member: Value): Result[Boolean] =
    execute(new Sadd(key, member.single).map(_ > 0))
  /** Executes [[http://redis.io/commands/sadd SADD]] */
  def sadd(key: Key, member: Value, members: Value*): Result[Int] =
    execute(new Sadd(key, member +:: members))
  /** Executes [[http://redis.io/commands/sadd SADD]]
    * or simply returns 0 when `members` is empty */
  def sadd(key: Key, members: Iterable[Value]): Result[Int] =
    execute(new Sadd(key, members))
  /** Executes [[http://redis.io/commands/scard SCARD]] */
  def scard(key: Key): Result[Long] =
    execute(new Scard(key))
  /** Executes [[http://redis.io/commands/sdiff SDIFF]] */
  def sdiff(source: Key, keys: Key*): Result[BSet[Value]] =
    execute(new Sdiff(source, keys))
  /** Executes [[http://redis.io/commands/sdiff SDIFF]] */
  def sdiff(source: Key, keys: Iterable[Key]): Result[BSet[Value]] =
    execute(new Sdiff(source, keys))
  /** Executes [[http://redis.io/commands/sdiffstore SDIFFSTORE]] */
  def sdiffstore(destination: Key, source: Key, keys: Key*): Result[Long] =
    execute(new Sdiffstore(destination, source, keys))
  /** Executes [[http://redis.io/commands/sdiffstore SDIFFSTORE]] */
  def sdiffstore(destination: Key, source: Key, keys: Iterable[Key]): Result[Long] =
    execute(new Sdiffstore(destination, source, keys))
  /** Executes [[http://redis.io/commands/sinter SINTER]] */
  def sinter(key: Key, keys: Key*): Result[BSet[Value]] =
    execute(new Sinter(key +:: keys))
  /** Executes [[http://redis.io/commands/sinter SINTER]]
    * NOTE: `keys` MUST NOT be empty */
  def sinter(keys: Iterable[Key]): Result[BSet[Value]] =
    execute(new Sinter(keys))
  /** Executes [[http://redis.io/commands/sinterstore SINTERSTORE]] */
  def sinterstore(destination: Key, key: Key, keys: Key*): Result[Long] =
    execute(new Sinterstore(destination, key +:: keys))
  /** Executes [[http://redis.io/commands/sinterstore SINTERSTORE]]
    * NOTE: `keys` MUST NOT be empty */
  def sinterstore(destination: Key, keys: Iterable[Key]): Result[Long] =
    execute(new Sinterstore(destination, keys))
  /** Executes [[http://redis.io/commands/sismember SISMEMBER]] */
  def sismember(key: Key, member: Value): Result[Boolean] =
    execute(new Sismember(key, member))
  /** Executes [[http://redis.io/commands/smembers SMEMBERS]] */
  def smembers(key: Key): Result[BSet[Value]] =
    execute(new Smembers(key))
  /** Executes [[http://redis.io/commands/smove SMOVE]] */
  def smove(source: Key, destination: Key, member: Value): Result[Boolean] =
    execute(new Smove(source, destination, member))
  /** Executes [[http://redis.io/commands/spop SPOP]] */
  def spop(key: Key): Result[Opt[Value]] =
    execute(new Spop(key))
  /** Executes [[http://redis.io/commands/spop SPOP]] */
  def spop(key: Key, count: Int): Result[BSet[Value]] =
    execute(new SpopCount(key, count))
  /** Executes [[http://redis.io/commands/srandmember SRANDMEMBER]] */
  def srandmember(key: Key): Result[Opt[Value]] =
    execute(new Srandmember(key))
  /** Executes [[http://redis.io/commands/srandmember SRANDMEMBER]] */
  def srandmember(key: Key, count: Int): Result[Seq[Value]] =
    execute(new SrandmemberCount(key, count))
  /** Executes [[http://redis.io/commands/srandmember SRANDMEMBER]] */
  def srandmemberDistinct(key: Key, count: Int): Result[BSet[Value]] =
    execute(new SrandmemberCountDistinct(key, count))
  /** Executes [[http://redis.io/commands/srem SREM]] */
  def srem(key: Key, member: Value): Result[Boolean] =
    execute(new Srem(key, member.single).map(_ > 0))
  /** Executes [[http://redis.io/commands/srem SREM]] */
  def srem(key: Key, member: Value, members: Value*): Result[Int] =
    execute(new Srem(key, member +:: members))
  /** Executes [[http://redis.io/commands/srem SREM]]
    * or simply returns 0 when `members` is empty */
  def srem(key: Key, members: Iterable[Value]): Result[Int] =
    execute(new Srem(key, members))
  /** Executes [[http://redis.io/commands/sscan SSCAN]] */
  def sscan(key: Key, cursor: Cursor, matchPattern: OptArg[Value] = OptArg.Empty, count: OptArg[Int] = OptArg.Empty): Result[(Cursor, Seq[Value])] =
    execute(new Sscan(key, cursor, matchPattern.toOpt, count.toOpt))
  /** Executes [[http://redis.io/commands/sunion SUNION]] */
  def sunion(key: Key, keys: Key*): Result[BSet[Value]] =
    execute(new Sunion(key +:: keys))
  /** Executes [[http://redis.io/commands/sunion SUNION]]
    * or simply returns empty set when `keys` is empty */
  def sunion(keys: Iterable[Key]): Result[BSet[Value]] =
    execute(new Sunion(keys))
  /** Executes [[http://redis.io/commands/sunionstore SUNIONSTORE]] */
  def sunionstore(destination: Key, key: Key, keys: Key*): Result[Long] =
    execute(new Sunionstore(destination, key +:: keys))
  /** Executes [[http://redis.io/commands/sunionstore SUNIONSTORE]]
    * NOTE: unlike in `SUNION` `keys` MUST NOT be empty because `SUNIONSTORE` overwrites destination key.
    * An operation equivalent to invoking `SUNIONSTORE` with only `destination` and no `keys` would be `DEL destination` */
  def sunionstore(destination: Key, keys: Iterable[Key]): Result[Long] =
    execute(new Sunionstore(destination, keys))

  private final class Sadd(key: Key, members: Iterable[Value]) extends RedisIntCommand with NodeCommand {
    val encoded: Encoded = encoder("SADD").key(key).datas(members).result
    override def immediateResult: Opt[Int] = whenEmpty(members, 0)
  }

  private final class Scard(key: Key) extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("SCARD").key(key).result
  }

  private final class Sdiff(source: Key, keys: Iterable[Key]) extends RedisDataSetCommand[Value] with NodeCommand {
    val encoded: Encoded = encoder("SDIFF").key(source).keys(keys).result
  }

  private final class Sdiffstore(destination: Key, source: Key, keys: Iterable[Key]) extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("SDIFFSTORE").key(destination).key(source).keys(keys).result
  }

  private final class Sinter(keys: Iterable[Key]) extends RedisDataSetCommand[Value] with NodeCommand {
    val encoded: Encoded = encoder("SINTER").keys(keys).result
  }

  private final class Sinterstore(destination: Key, keys: Iterable[Key]) extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("SINTERSTORE").key(destination).keys(keys).result
  }

  private final class Sismember(key: Key, member: Value) extends RedisBooleanCommand with NodeCommand {
    val encoded: Encoded = encoder("SISMEMBER").key(key).data(member).result
  }

  private final class Smembers(key: Key) extends RedisDataSetCommand[Value] with NodeCommand {
    val encoded: Encoded = encoder("SMEMBERS").key(key).result
  }

  private final class Smove(source: Key, destination: Key, member: Value) extends RedisBooleanCommand with NodeCommand {
    val encoded: Encoded = encoder("SMOVE").key(source).key(destination).data(member).result
  }

  private final class Spop(key: Key) extends RedisOptDataCommand[Value] with NodeCommand {
    val encoded: Encoded = encoder("SPOP").key(key).result
  }

  private final class SpopCount(key: Key, count: Int) extends RedisDataSetCommand[Value] with NodeCommand {
    val encoded: Encoded = encoder("SPOP").key(key).add(count).result
  }

  private final class Srandmember(key: Key) extends RedisOptDataCommand[Value] with NodeCommand {
    val encoded: Encoded = encoder("SRANDMEMBER").key(key).result
  }

  private final class SrandmemberCount(key: Key, count: Int) extends RedisDataSeqCommand[Value] with NodeCommand {
    require(count >= 0, "count cannot be negative")
    val encoded: Encoded = encoder("SRANDMEMBER").key(key).add(-count).result
  }

  private final class SrandmemberCountDistinct(key: Key, count: Int) extends RedisDataSetCommand[Value] with NodeCommand {
    require(count >= 0, "count cannot be negative")
    val encoded: Encoded = encoder("SRANDMEMBER").key(key).add(count).result
  }

  private final class Srem(key: Key, members: Iterable[Value]) extends RedisIntCommand with NodeCommand {
    val encoded: Encoded = encoder("SREM").key(key).datas(members).result
    override def immediateResult: Opt[Int] = whenEmpty(members, 0)
  }

  private final class Sscan(key: Key, cursor: Cursor, matchPattern: Opt[Value], count: Opt[Int])
    extends RedisScanCommand[Value](multiBulkSeq[Value]) with NodeCommand {
    val encoded: Encoded = encoder("SSCAN").key(key).add(cursor.raw).optData("MATCH", matchPattern).optAdd("COUNT", count).result
  }

  private final class Sunion(keys: Iterable[Key]) extends RedisDataSetCommand[Value] with NodeCommand {
    val encoded: Encoded = encoder("SUNION").keys(keys).result
    override def immediateResult: Opt[BSet[Value]] = whenEmpty(keys, Set.empty)
  }

  private final class Sunionstore(destination: Key, keys: Iterable[Key]) extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("SUNIONSTORE").key(destination).keys(keys).result
  }
}
