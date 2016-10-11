package com.avsystem.commons
package redis.commands

import com.avsystem.commons.misc.{Opt, OptArg}
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.commands.ReplyDecoders._

trait SetsApi extends ApiSubset {
  def sadd(key: Key, members: Value*): Result[Long] =
    execute(new Sadd(key, members))
  def scard(key: Key): Result[Long] =
    execute(new Scard(key))
  def sdiff(source: Key, keys: Key*): Result[Seq[Value]] =
    execute(new Sdiff(source, keys))
  def sdiffstore(destination: Key, source: Key, keys: Key*): Result[Long] =
    execute(new Sdiffstore(destination, source, keys))
  def sinter(keys: Key*): Result[Seq[Value]] =
    execute(new Sinter(keys))
  def sinterstore(destination: Key, keys: Key*): Result[Long] =
    execute(new Sinterstore(destination, keys))
  def sismember(key: Key, member: Value): Result[Boolean] =
    execute(new Sismember(key, member))
  def smembers(key: Key): Result[Seq[Value]] =
    execute(new Smembers(key))
  def smove(source: Key, destination: Key, member: Value): Result[Boolean] =
    execute(new Smove(source, destination, member))
  def spop(key: Key): Result[Opt[Value]] =
    execute(new Spop(key))
  def spop(key: Key, count: Long): Result[Seq[Value]] =
    execute(new SpopCount(key, count))
  def srandmember(key: Key): Result[Opt[Value]] =
    execute(new Srandmember(key))
  def srandmember(key: Key, count: Long, distinct: Boolean = true): Result[Seq[Value]] =
    execute(new SrandmemberCount(key, count, distinct))
  def srem(key: Key, members: Value*): Result[Long] =
    execute(new Srem(key, members))
  def sscan(key: Key, cursor: Cursor, matchPattern: OptArg[Value] = OptArg.Empty, count: OptArg[Long] = OptArg.Empty): Result[(Cursor, Seq[Value])] =
    execute(new Sscan(key, cursor, matchPattern.toOpt, count.toOpt))
  def sunion(keys: Key*): Result[Seq[Value]] =
    execute(new Sunion(keys))
  def sunionstore(destination: Key, keys: Key*): Result[Long] =
    execute(new Sunionstore(destination, keys))

  private final class Sadd(key: Key, members: Seq[Value]) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("SADD").key(key).datas(members).result
  }

  private final class Scard(key: Key) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("SCARD").key(key).result
  }

  private final class Sdiff(source: Key, keys: Seq[Key]) extends RedisDataSeqCommand[Value] with NodeCommand {
    val encoded = encoder("SDIFF").key(source).keys(keys).result
  }

  private final class Sdiffstore(destination: Key, source: Key, keys: Seq[Key]) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("SDIFFSTORE").key(destination).key(source).keys(keys).result
  }

  private final class Sinter(keys: Seq[Key]) extends RedisDataSeqCommand[Value] with NodeCommand {
    val encoded = encoder("SINTER").keys(keys).result
  }

  private final class Sinterstore(destination: Key, keys: Seq[Key]) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("SINTERSTORE").key(destination).keys(keys).result
  }

  private final class Sismember(key: Key, member: Value) extends RedisBooleanCommand with NodeCommand {
    val encoded = encoder("SISMEMBER").key(key).data(member).result
  }

  private final class Smembers(key: Key) extends RedisDataSeqCommand[Value] with NodeCommand {
    val encoded = encoder("SMEMBERS").key(key).result
  }

  private final class Smove(source: Key, destination: Key, member: Value) extends RedisBooleanCommand with NodeCommand {
    val encoded = encoder("SMOVE").key(source).key(destination).data(member).result
  }

  private final class Spop(key: Key) extends RedisOptDataCommand[Value] with NodeCommand {
    val encoded = encoder("SPOP").key(key).result
  }

  private final class SpopCount(key: Key, count: Long) extends RedisDataSeqCommand[Value] with NodeCommand {
    val encoded = encoder("SPOP").key(key).add(count).result
  }

  private final class Srandmember(key: Key) extends RedisOptDataCommand[Value] with NodeCommand {
    val encoded = encoder("SRANDMEMBER").key(key).result
  }

  private final class SrandmemberCount(key: Key, count: Long, distinct: Boolean) extends RedisDataSeqCommand[Value] with NodeCommand {
    val encoded = encoder("SRANDMEMBER").key(key).add(if (distinct) count else -count).result
  }

  private final class Srem(key: Key, members: Seq[Value]) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("SREM").key(key).datas(members).result
  }

  private final class Sscan(key: Key, cursor: Cursor, matchPattern: Opt[Value], count: Opt[Long])
    extends RedisScanCommand[Value](multiBulk[Value]) with NodeCommand {
    val encoded = encoder("SSCAN").key(key).add(cursor.raw).optData("MATCH", matchPattern).optAdd("COUNT", count).result
  }

  private final class Sunion(keys: Seq[Key]) extends RedisDataSeqCommand[Value] with NodeCommand {
    val encoded = encoder("SUNION").keys(keys).result
  }

  private final class Sunionstore(destination: Key, keys: Seq[Key]) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("SUNIONSTORE").key(destination).keys(keys).result
  }
}
