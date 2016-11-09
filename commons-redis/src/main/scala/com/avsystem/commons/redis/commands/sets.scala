package com.avsystem.commons
package redis.commands

import com.avsystem.commons.collection.CollectionAliases.BSet
import com.avsystem.commons.misc.{Opt, OptArg}
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.commands.ReplyDecoders._

trait SetsApi extends ApiSubset {
  /** [[http://redis.io/commands/sadd SADD]] */
  def sadd(key: Key, member: Value): Result[Boolean] =
    execute(new Sadd(key, member.single).map(_ > 0))
  /** [[http://redis.io/commands/sadd SADD]] */
  def sadd(key: Key, member: Value, members: Value*): Result[Int] =
    execute(new Sadd(key, member +:: members))
  /** [[http://redis.io/commands/sadd SADD]] */
  def sadd(key: Key, members: Iterable[Value]): Result[Int] =
    execute(new Sadd(key, members))
  /** [[http://redis.io/commands/scard SCARD]] */
  def scard(key: Key): Result[Long] =
    execute(new Scard(key))
  /** [[http://redis.io/commands/sdiff SDIFF]] */
  def sdiff(source: Key, keys: Key*): Result[BSet[Value]] =
    execute(new Sdiff(source, keys))
  /** [[http://redis.io/commands/sdiff SDIFF]] */
  def sdiff(source: Key, keys: Iterable[Key]): Result[BSet[Value]] =
    execute(new Sdiff(source, keys))
  /** [[http://redis.io/commands/sdiffstore SDIFFSTORE]] */
  def sdiffstore(destination: Key, source: Key, keys: Key*): Result[Long] =
    execute(new Sdiffstore(destination, source, keys))
  /** [[http://redis.io/commands/sdiffstore SDIFFSTORE]] */
  def sdiffstore(destination: Key, source: Key, keys: Iterable[Key]): Result[Long] =
    execute(new Sdiffstore(destination, source, keys))
  /** [[http://redis.io/commands/sinter SINTER]] */
  def sinter(key: Key, keys: Key*): Result[BSet[Value]] =
    execute(new Sinter(key +:: keys))
  /** [[http://redis.io/commands/sinter SINTER]] */
  def sinter(keys: Iterable[Key]): Result[BSet[Value]] =
    execute(new Sinter(keys))
  /** [[http://redis.io/commands/sinterstore SINTERSTORE]] */
  def sinterstore(destination: Key, key: Key, keys: Key*): Result[Long] =
    execute(new Sinterstore(destination, key +:: keys))
  /** [[http://redis.io/commands/sinterstore SINTERSTORE]] */
  def sinterstore(destination: Key, keys: Iterable[Key]): Result[Long] =
    execute(new Sinterstore(destination, keys))
  /** [[http://redis.io/commands/sismember SISMEMBER]] */
  def sismember(key: Key, member: Value): Result[Boolean] =
    execute(new Sismember(key, member))
  /** [[http://redis.io/commands/smembers SMEMBERS]] */
  def smembers(key: Key): Result[BSet[Value]] =
    execute(new Smembers(key))
  /** [[http://redis.io/commands/smove SMOVE]] */
  def smove(source: Key, destination: Key, member: Value): Result[Boolean] =
    execute(new Smove(source, destination, member))
  /** [[http://redis.io/commands/spop SPOP]] */
  def spop(key: Key): Result[Opt[Value]] =
    execute(new Spop(key))
  /** [[http://redis.io/commands/spop SPOP]] */
  def spop(key: Key, count: Int): Result[BSet[Value]] =
    execute(new SpopCount(key, count))
  /** [[http://redis.io/commands/srandmember SRANDMEMBER]] */
  def srandmember(key: Key): Result[Opt[Value]] =
    execute(new Srandmember(key))
  /** [[http://redis.io/commands/srandmember SRANDMEMBER]] */
  def srandmember(key: Key, count: Int): Result[Seq[Value]] =
    execute(new SrandmemberCount(key, count))
  /** [[http://redis.io/commands/srandmember SRANDMEMBER]] */
  def srandmemberDistinct(key: Key, count: Int): Result[BSet[Value]] =
    execute(new SrandmemberCountDistinct(key, count))
  /** [[http://redis.io/commands/srem SREM]] */
  def srem(key: Key, member: Value): Result[Boolean] =
    execute(new Srem(key, member.single).map(_ > 0))
  /** [[http://redis.io/commands/srem SREM]] */
  def srem(key: Key, member: Value, members: Value*): Result[Int] =
    execute(new Srem(key, member +:: members))
  /** [[http://redis.io/commands/srem SREM]] */
  def srem(key: Key, members: Iterable[Value]): Result[Int] =
    execute(new Srem(key, members))
  /** [[http://redis.io/commands/sscan SSCAN]] */
  def sscan(key: Key, cursor: Cursor, matchPattern: OptArg[Value] = OptArg.Empty, count: OptArg[Int] = OptArg.Empty): Result[(Cursor, Seq[Value])] =
    execute(new Sscan(key, cursor, matchPattern.toOpt, count.toOpt))
  /** [[http://redis.io/commands/sunion SUNION]] */
  def sunion(key: Key, keys: Key*): Result[BSet[Value]] =
    execute(new Sunion(key +:: keys))
  /** [[http://redis.io/commands/sunion SUNION]] */
  def sunion(keys: Iterable[Key]): Result[BSet[Value]] =
    execute(new Sunion(keys))
  /** [[http://redis.io/commands/sunionstore SUNIONSTORE]] */
  def sunionstore(destination: Key, key: Key, keys: Key*): Result[Long] =
    execute(new Sunionstore(destination, key +:: keys))
  /** [[http://redis.io/commands/sunionstore SUNIONSTORE]] */
  def sunionstore(destination: Key, keys: Iterable[Key]): Result[Long] =
    execute(new Sunionstore(destination, keys))

  private final class Sadd(key: Key, members: Iterable[Value]) extends RedisIntCommand with NodeCommand {
    requireNonEmpty(members, "members")
    val encoded = encoder("SADD").key(key).datas(members).result
  }

  private final class Scard(key: Key) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("SCARD").key(key).result
  }

  private final class Sdiff(source: Key, keys: Iterable[Key]) extends RedisDataSetCommand[Value] with NodeCommand {
    val encoded = encoder("SDIFF").key(source).keys(keys).result
  }

  private final class Sdiffstore(destination: Key, source: Key, keys: Iterable[Key]) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("SDIFFSTORE").key(destination).key(source).keys(keys).result
  }

  private final class Sinter(keys: Iterable[Key]) extends RedisDataSetCommand[Value] with NodeCommand {
    requireNonEmpty(keys, "keys")
    val encoded = encoder("SINTER").keys(keys).result
  }

  private final class Sinterstore(destination: Key, keys: Iterable[Key]) extends RedisLongCommand with NodeCommand {
    requireNonEmpty(keys, "keys")
    val encoded = encoder("SINTERSTORE").key(destination).keys(keys).result
  }

  private final class Sismember(key: Key, member: Value) extends RedisBooleanCommand with NodeCommand {
    val encoded = encoder("SISMEMBER").key(key).data(member).result
  }

  private final class Smembers(key: Key) extends RedisDataSetCommand[Value] with NodeCommand {
    val encoded = encoder("SMEMBERS").key(key).result
  }

  private final class Smove(source: Key, destination: Key, member: Value) extends RedisBooleanCommand with NodeCommand {
    val encoded = encoder("SMOVE").key(source).key(destination).data(member).result
  }

  private final class Spop(key: Key) extends RedisOptDataCommand[Value] with NodeCommand {
    val encoded = encoder("SPOP").key(key).result
  }

  private final class SpopCount(key: Key, count: Int) extends RedisDataSetCommand[Value] with NodeCommand {
    val encoded = encoder("SPOP").key(key).add(count).result
  }

  private final class Srandmember(key: Key) extends RedisOptDataCommand[Value] with NodeCommand {
    val encoded = encoder("SRANDMEMBER").key(key).result
  }

  private final class SrandmemberCount(key: Key, count: Int) extends RedisDataSeqCommand[Value] with NodeCommand {
    require(count >= 0, "count cannot be negative")
    val encoded = encoder("SRANDMEMBER").key(key).add(-count).result
  }

  private final class SrandmemberCountDistinct(key: Key, count: Int) extends RedisDataSetCommand[Value] with NodeCommand {
    require(count >= 0, "count cannot be negative")
    val encoded = encoder("SRANDMEMBER").key(key).add(count).result
  }

  private final class Srem(key: Key, members: Iterable[Value]) extends RedisIntCommand with NodeCommand {
    requireNonEmpty(members, "members")
    val encoded = encoder("SREM").key(key).datas(members).result
  }

  private final class Sscan(key: Key, cursor: Cursor, matchPattern: Opt[Value], count: Opt[Int])
    extends RedisScanCommand[Value](multiBulkSeq[Value]) with NodeCommand {
    val encoded = encoder("SSCAN").key(key).add(cursor.raw).optData("MATCH", matchPattern).optAdd("COUNT", count).result
  }

  private final class Sunion(keys: Iterable[Key]) extends RedisDataSetCommand[Value] with NodeCommand {
    requireNonEmpty(keys, "keys")
    val encoded = encoder("SUNION").keys(keys).result
  }

  private final class Sunionstore(destination: Key, keys: Iterable[Key]) extends RedisLongCommand with NodeCommand {
    requireNonEmpty(keys, "keys")
    val encoded = encoder("SUNIONSTORE").key(destination).keys(keys).result
  }
}
