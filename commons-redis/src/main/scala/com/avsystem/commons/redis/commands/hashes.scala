package com.avsystem.commons
package redis.commands

import com.avsystem.commons.collection.CollectionAliases.{BMap, BSet}
import com.avsystem.commons.misc.{Opt, OptArg}
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.commands.ReplyDecoders._

trait HashesApi extends ApiSubset {
  /** [[http://redis.io/commands/hdel HDEL]] */
  def hdel(key: Key, field: HashKey): Result[Boolean] =
    execute(new Hdel(key, field.single).map(_ > 0))
  /** [[http://redis.io/commands/hdel HDEL]] */
  def hdel(key: Key, field: HashKey, fields: HashKey*): Result[Int] =
    execute(new Hdel(key, field +:: fields))
  /** [[http://redis.io/commands/hdel HDEL]] */
  def hdel(key: Key, fields: Iterable[HashKey]): Result[Int] =
    execute(new Hdel(key, fields))
  /** [[http://redis.io/commands/hexists HEXISTS]] */
  def hexists(key: Key, field: HashKey): Result[Boolean] =
    execute(new Hexists(key, field))
  /** [[http://redis.io/commands/hget HGET]] */
  def hget(key: Key, field: HashKey): Result[Opt[Value]] =
    execute(new Hget(key, field))
  /** [[http://redis.io/commands/hgetall HGETALL]] */
  def hgetall(key: Key): Result[BMap[HashKey, Value]] =
    execute(new Hgetall(key))
  /** [[http://redis.io/commands/hincrby HINCRBY]] */
  def hincrby(key: Key, field: HashKey, increment: Long): Result[Long] =
    execute(new Hincrby(key, field, increment))
  /** [[http://redis.io/commands/hincrbyfloat HINCRBYFLOAT]] */
  def hincrbyfloat(key: Key, field: HashKey, increment: Double): Result[Double] =
    execute(new Hincrbyfloat(key, field, increment))
  /** [[http://redis.io/commands/hkeys HKEYS]] */
  def hkeys(key: Key): Result[BSet[HashKey]] =
    execute(new Hkeys(key))
  /** [[http://redis.io/commands/hlen HLEN]] */
  def hlen(key: Key): Result[Long] =
    execute(new Hlen(key))
  /** [[http://redis.io/commands/hmget HMGET]] */
  def hmget(key: Key, field: HashKey, fields: HashKey*): Result[Seq[Opt[Value]]] =
    execute(new Hmget(key, field +:: fields))
  /** [[http://redis.io/commands/hmget HMGET]] */
  def hmget(key: Key, fields: Seq[HashKey]): Result[Seq[Opt[Value]]] =
    execute(new Hmget(key, fields))
  /** [[http://redis.io/commands/hmset HMSET]] */
  def hmset(key: Key, fieldValue: (HashKey, Value), fieldValues: (HashKey, Value)*): Result[Unit] =
    execute(new Hmset(key, fieldValue +:: fieldValues))
  /** [[http://redis.io/commands/hmset HMSET]] */
  def hmset(key: Key, fieldValues: Iterable[(HashKey, Value)]): Result[Unit] =
    execute(new Hmset(key, fieldValues))
  /** [[http://redis.io/commands/hscan HSCAN]] */
  def hscan(key: Key, cursor: Cursor, matchPattern: OptArg[HashKey] = OptArg.Empty, count: OptArg[Int] = OptArg.Empty): Result[(Cursor, Seq[(HashKey, Value)])] =
    execute(new Hscan(key, cursor, matchPattern.toOpt, count.toOpt))
  /** [[http://redis.io/commands/hset HSET]] */
  def hset(key: Key, field: HashKey, value: Value): Result[Boolean] =
    execute(new Hset(key, field, value))
  /** [[http://redis.io/commands/hsetnx HSETNX]] */
  def hsetnx(key: Key, field: HashKey, value: Value): Result[Boolean] =
    execute(new Hsetnx(key, field, value))
  /** [[http://redis.io/commands/hstrlen HSTRLEN]] */
  def hstrlen(key: Key, field: HashKey): Result[Int] =
    execute(new Hstrlen(key, field))
  /** [[http://redis.io/commands/hvals HVALS]] */
  def hvals(key: Key): Result[Seq[Value]] =
    execute(new Hvals(key))

  private final class Hdel(key: Key, fields: Iterable[HashKey]) extends RedisIntCommand with NodeCommand {
    requireNonEmpty(fields, "fields")
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
    extends AbstractRedisCommand[BMap[HashKey, Value]](mapMultiBulk[HashKey, Value]) with NodeCommand {
    val encoded = encoder("HGETALL").key(key).result
  }

  private final class Hincrby(key: Key, field: HashKey, increment: Long) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("HINCRBY").key(key).data(field).add(increment).result
  }

  private final class Hincrbyfloat(key: Key, field: HashKey, increment: Double) extends RedisDoubleCommand with NodeCommand {
    val encoded = encoder("HINCRBYFLOAT").key(key).data(field).add(increment).result
  }

  private final class Hkeys(key: Key) extends RedisDataSetCommand[HashKey] with NodeCommand {
    val encoded = encoder("HKEYS").key(key).result
  }

  private final class Hlen(key: Key) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("HLEN").key(key).result
  }

  private final class Hmget(key: Key, fields: Iterable[HashKey])
    extends RedisOptDataSeqCommand[Value] with NodeCommand {
    requireNonEmpty(fields, "fields")
    val encoded = encoder("HMGET").key(key).datas(fields).result
  }

  private final class Hmset(key: Key, fieldValues: Iterable[(HashKey, Value)]) extends RedisUnitCommand with NodeCommand {
    requireNonEmpty(fieldValues, "fieldValues")
    val encoded = encoder("HMSET").key(key).dataPairs(fieldValues).result
  }

  private final class Hscan(key: Key, cursor: Cursor, matchPattern: Opt[HashKey], count: Opt[Int])
    extends RedisScanCommand[(HashKey, Value)](pairedMultiBulk[HashKey, Value]) with NodeCommand {
    val encoded = encoder("HSCAN").key(key).add(cursor.raw).optData("MATCH", matchPattern).optAdd("COUNT", count).result
  }

  private final class Hset(key: Key, field: HashKey, value: Value) extends RedisBooleanCommand with NodeCommand {
    val encoded = encoder("HSET").key(key).data(field).data(value).result
  }

  private final class Hsetnx(key: Key, field: HashKey, value: Value) extends RedisBooleanCommand with NodeCommand {
    val encoded = encoder("HSETNX").key(key).data(field).data(value).result
  }

  private final class Hstrlen(key: Key, field: HashKey) extends RedisIntCommand with NodeCommand {
    val encoded = encoder("HSTRLEN").key(key).data(field).result
  }

  private final class Hvals(key: Key) extends RedisDataSeqCommand[Value] with NodeCommand {
    val encoded = encoder("HVALS").key(key).result
  }
}
