package com.avsystem.commons
package redis.commands

import com.avsystem.commons.misc.{Opt, OptArg}
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.commands.ReplyDecoders._

trait HashesApi extends ApiSubset {
  def hdel(key: Key, fields: HashKey*): Result[Long] =
    execute(new Hdel(key, fields))
  def hexists(key: Key, field: HashKey): Result[Boolean] =
    execute(new Hexists(key, field))
  def hget(key: Key, field: HashKey): Result[Opt[Value]] =
    execute(new Hget(key, field))
  def hgetall(key: Key): Result[Seq[(HashKey, Value)]] =
    execute(new Hgetall(key))
  def hincrby(key: Key, field: HashKey, increment: Long): Result[Long] =
    execute(new Hincrby(key, field, increment))
  def hincrbyfloat(key: Key, field: HashKey, increment: Double): Result[Double] =
    execute(new Hincrbyfloat(key, field, increment))
  def hkeys(key: Key): Result[Seq[HashKey]] =
    execute(new Hkeys(key))
  def hlen(key: Key): Result[Long] =
    execute(new Hlen(key))
  def hmget(key: Key, fields: HashKey*): Result[Seq[Opt[Value]]] =
    execute(new Hmget(key, fields))
  def hmset(key: Key, fieldValues: (HashKey, Value)*): Result[Unit] =
    execute(new Hmset(key, fieldValues))
  def hscan(key: Key, cursor: Cursor, matchPattern: OptArg[HashKey] = OptArg.Empty, count: OptArg[Long] = OptArg.Empty): Result[(Cursor, Seq[(HashKey, Value)])] =
    execute(new Hscan(key, cursor, matchPattern.toOpt, count.toOpt))
  def hset(key: Key, field: HashKey, value: Value): Result[Boolean] =
    execute(new Hset(key, field, value))
  def hsetnx(key: Key, field: HashKey, value: Value): Result[Boolean] =
    execute(new Hsetnx(key, field, value))
  def hstrlen(key: Key, field: HashKey): Result[Long] =
    execute(new Hstrlen(key, field))
  def hvals(key: Key): Result[Seq[Value]] =
    execute(new Hvals(key))

  private final class Hdel(key: Key, fields: Seq[HashKey]) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("HDEL").key(key).datas(fields).result
  }

  private final class Hexists(key: Key, field: HashKey) extends RedisBooleanCommand with NodeCommand {
    val encoded = encoder("HEXISTS").key(key).data(field).result
  }

  private final class Hget(key: Key, field: HashKey)
    extends RedisOptDataCommand[Value] with NodeCommand {
    val encoded = encoder("HGET").key(key).data(field).result
  }

  private final class Hgetall(key: Key)
    extends AbstractRedisCommand[Seq[(HashKey, Value)]](pairedMultiBulk[HashKey, Value]) with NodeCommand {
    val encoded = encoder("HGETALL").key(key).result
  }

  private final class Hincrby(key: Key, field: HashKey, increment: Long) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("HINCRBY").key(key).data(field).add(increment).result
  }

  private final class Hincrbyfloat(key: Key, field: HashKey, increment: Double) extends RedisDoubleCommand with NodeCommand {
    val encoded = encoder("HINCRBYFLOAT").key(key).data(field).add(increment).result
  }

  private final class Hkeys(key: Key) extends RedisDataSeqCommand[HashKey] with NodeCommand {
    val encoded = encoder("HKEYS").key(key).result
  }

  private final class Hlen(key: Key) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("HLEN").key(key).result
  }

  private final class Hmget(key: Key, fields: Seq[HashKey])
    extends RedisOptDataSeqCommand[Value] with NodeCommand {
    val encoded = encoder("HMGET").key(key).datas(fields).result
  }

  private final class Hmset(key: Key, fieldValues: Seq[(HashKey, Value)]) extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("HMSET").key(key).dataPairs(fieldValues).result
  }

  private final class Hscan(key: Key, cursor: Cursor, matchPattern: Opt[HashKey], count: Opt[Long])
    extends RedisScanCommand[(HashKey, Value)](pairedMultiBulk[HashKey, Value]) with NodeCommand {
    val encoded = encoder("HSCAN").key(key).add(cursor.raw).optData("MATCH", matchPattern).optAdd("COUNT", count).result
  }

  private final class Hset(key: Key, field: HashKey, value: Value) extends RedisBooleanCommand with NodeCommand {
    val encoded = encoder("HSET").key(key).data(field).data(value).result
  }

  private final class Hsetnx(key: Key, field: HashKey, value: Value) extends RedisBooleanCommand with NodeCommand {
    val encoded = encoder("HSETNX").key(key).data(field).data(value).result
  }

  private final class Hstrlen(key: Key, field: HashKey) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("HSTRLEN").key(key).data(field).result
  }

  private final class Hvals(key: Key) extends RedisDataSeqCommand[Value] with NodeCommand {
    val encoded = encoder("HVALS").key(key).result
  }
}
