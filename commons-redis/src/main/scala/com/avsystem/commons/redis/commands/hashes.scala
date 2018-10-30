package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis._
import com.avsystem.commons.redis.commands.ReplyDecoders._

trait HashesApi extends FieldValueApiSubset with RecordApiSubset {
  /** Executes [[http://redis.io/commands/hdel HDEL]] */
  def hdel(key: Key, field: Field): Result[Boolean] =
    execute(new Hdel(key, field.single).map(_ > 0))
  /** Executes [[http://redis.io/commands/hdel HDEL]] */
  def hdel(key: Key, field: Field, fields: Field*): Result[Int] =
    execute(new Hdel(key, field +:: fields))
  /** Executes [[http://redis.io/commands/hdel HDEL]]
    * or simply returns 0 when `fields` is empty, without sending the command to Redis */
  def hdel(key: Key, fields: Iterable[Field]): Result[Int] =
    execute(new Hdel(key, fields))
  /** Executes [[http://redis.io/commands/hexists HEXISTS]] */
  def hexists(key: Key, field: Field): Result[Boolean] =
    execute(new Hexists(key, field))
  /** Executes [[http://redis.io/commands/hget HGET]] */
  def hget(key: Key, field: Field): Result[Opt[Value]] =
    execute(new Hget(key, field))
  /** Executes [[http://redis.io/commands/hgetall HGETALL]] */
  def hgetall(key: Key): Result[BMap[Field, Value]] =
    execute(new Hgetall(key))
  /** Executes [[http://redis.io/commands/hgetall HGETALL]] */
  def hgetallRecord(key: Key): Result[Opt[Record]] =
    execute(new HgetallRecord(key))
  /** Executes [[http://redis.io/commands/hincrby HINCRBY]] */
  def hincrby(key: Key, field: Field, increment: Long): Result[Long] =
    execute(new Hincrby(key, field, increment))
  /** Executes [[http://redis.io/commands/hincrbyfloat HINCRBYFLOAT]] */
  def hincrbyfloat(key: Key, field: Field, increment: Double): Result[Double] =
    execute(new Hincrbyfloat(key, field, increment))
  /** Executes [[http://redis.io/commands/hkeys HKEYS]] */
  def hkeys(key: Key): Result[BSet[Field]] =
    execute(new Hkeys(key))
  /** Executes [[http://redis.io/commands/hlen HLEN]] */
  def hlen(key: Key): Result[Long] =
    execute(new Hlen(key))
  /** Executes [[http://redis.io/commands/hmget HMGET]] */
  def hmget(key: Key, field: Field, fields: Field*): Result[Seq[Opt[Value]]] =
    execute(new Hmget(key, field +:: fields))
  /** Executes [[http://redis.io/commands/hmget HMGET]]
    * or simply returns empty `Seq` when `fields` is empty, without sending the command to Redis */
  def hmget(key: Key, fields: Iterable[Field]): Result[Seq[Opt[Value]]] =
    execute(new Hmget(key, fields))
  /** Executes [[http://redis.io/commands/hmset HMSET]] */
  def hmset(key: Key, fieldValue: (Field, Value), fieldValues: (Field, Value)*): Result[Unit] =
    execute(new Hmset(key, fieldValue +:: fieldValues))
  /** Executes [[http://redis.io/commands/hmset HMSET]]
    * or does nothing when `fieldValues` is empty, without sending the command to Redis */
  def hmset(key: Key, fieldValues: Iterable[(Field, Value)]): Result[Unit] =
    execute(new Hmset(key, fieldValues))
  /** Executes [[http://redis.io/commands/hmset HMSET]]
    * or does nothing when `data` is empty, without sending the command to Redis */
  def hmsetRecord(key: Key, data: Record): Result[Unit] =
    execute(new HmsetRecord(key, data))
  /** Executes [[http://redis.io/commands/hscan HSCAN]] */
  def hscan(key: Key, cursor: Cursor, matchPattern: OptArg[Field] = OptArg.Empty, count: OptArg[Int] = OptArg.Empty): Result[(Cursor, Seq[(Field, Value)])] =
    execute(new Hscan(key, cursor, matchPattern.toOpt, count.toOpt))
  /** Executes [[http://redis.io/commands/hset HSET]] */
  def hset(key: Key, field: Field, value: Value): Result[Boolean] =
    execute(new Hset(key, (field, value).single).map(_ > 0))
  /** Executes [[http://redis.io/commands/hset HSET]] */
  def hset(key: Key, fieldValue: (Field, Value), fieldValues: (Field, Value)*): Result[Int] =
    execute(new Hset(key, fieldValue +:: fieldValues))
  /** Executes [[http://redis.io/commands/hset HSET]]
    * or does nothing when `fieldValues` is empty, without sending the command to Redis */
  def hset(key: Key, fieldValues: Iterable[(Field, Value)]): Result[Int] =
    execute(new Hset(key, fieldValues))
  /** Executes [[http://redis.io/commands/hset HSET]]
    * or does nothing when `data` is empty, without sending the command to Redis */
  def hsetRecord(key: Key, data: Record): Result[Int] =
    execute(new HsetRecord(key, data))
  /** Executes [[http://redis.io/commands/hsetnx HSETNX]] */
  def hsetnx(key: Key, field: Field, value: Value): Result[Boolean] =
    execute(new Hsetnx(key, field, value))
  /** Executes [[http://redis.io/commands/hstrlen HSTRLEN]] */
  def hstrlen(key: Key, field: Field): Result[Int] =
    execute(new Hstrlen(key, field))
  /** Executes [[http://redis.io/commands/hvals HVALS]] */
  def hvals(key: Key): Result[Iterable[Value]] =
    execute(new Hvals(key))

  private final class Hdel(key: Key, fields: Iterable[Field]) extends RedisIntCommand with NodeCommand {
    val encoded: Encoded = encoder("HDEL").key(key).datas(fields).result
    override def immediateResult: Opt[Int] = whenEmpty(fields, 0)
  }

  private final class Hexists(key: Key, field: Field) extends RedisBooleanCommand with NodeCommand {
    val encoded: Encoded = encoder("HEXISTS").key(key).data(field).result
  }

  private final class Hget(key: Key, field: Field)
    extends RedisOptDataCommand[Value] with NodeCommand {
    val encoded: Encoded = encoder("HGET").key(key).data(field).result
  }

  private final class Hgetall(key: Key)
    extends AbstractRedisCommand[BMap[Field, Value]](flatMultiBulkMap[Field, Value]) with NodeCommand {
    val encoded: Encoded = encoder("HGETALL").key(key).result
  }

  private final class HgetallRecord(key: Key)
    extends AbstractRedisCommand[Opt[Record]](flatMultiBulkRecordOpt) with NodeCommand {
    val encoded: Encoded = encoder("HGETALL").key(key).result
  }

  private final class Hincrby(key: Key, field: Field, increment: Long) extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("HINCRBY").key(key).data(field).add(increment).result
  }

  private final class Hincrbyfloat(key: Key, field: Field, increment: Double) extends RedisDoubleCommand with NodeCommand {
    val encoded: Encoded = encoder("HINCRBYFLOAT").key(key).data(field).add(increment).result
  }

  private final class Hkeys(key: Key) extends RedisDataSetCommand[Field] with NodeCommand {
    val encoded: Encoded = encoder("HKEYS").key(key).result
  }

  private final class Hlen(key: Key) extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("HLEN").key(key).result
  }

  private final class Hmget(key: Key, fields: Iterable[Field])
    extends RedisOptDataSeqCommand[Value] with NodeCommand {
    val encoded: Encoded = encoder("HMGET").key(key).datas(fields).result
    override def immediateResult: Opt[Seq[Opt[Value]]] = whenEmpty(fields, Seq.empty)
  }

  private final class Hmset(key: Key, fieldValues: Iterable[(Field, Value)]) extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("HMSET").key(key).dataPairs(fieldValues).result
    override def immediateResult: Opt[Unit] = whenEmpty(fieldValues, ())
  }

  private final class HmsetRecord(key: Key, data: Record) extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("HMSET").key(key).dataPairs(data).result
    override def immediateResult: Opt[Unit] = if (encoded.elements.size <= 2) Opt(()) else Opt.Empty
  }

  private final class Hscan(key: Key, cursor: Cursor, matchPattern: Opt[Field], count: Opt[Int])
    extends RedisScanCommand[(Field, Value)](flatMultiBulkSeq[Field, Value]) with NodeCommand {
    val encoded: Encoded = encoder("HSCAN").key(key).add(cursor.raw).optData("MATCH", matchPattern).optAdd("COUNT", count).result
  }

  private final class Hset(key: Key, fieldValues: Iterable[(Field, Value)]) extends RedisIntCommand with NodeCommand {
    val encoded: Encoded = encoder("HSET").key(key).dataPairs(fieldValues).result
    override def immediateResult: Opt[Int] = whenEmpty(fieldValues, 0)
  }

  private final class HsetRecord(key: Key, data: Record) extends RedisIntCommand with NodeCommand {
    val encoded: Encoded = encoder("HSET").key(key).dataPairs(data).result
    override def immediateResult: Opt[Int] = if (encoded.elements.size <= 2) Opt(0) else Opt.Empty
  }

  private final class Hsetnx(key: Key, field: Field, value: Value) extends RedisBooleanCommand with NodeCommand {
    val encoded: Encoded = encoder("HSETNX").key(key).data(field).data(value).result
  }

  private final class Hstrlen(key: Key, field: Field) extends RedisIntCommand with NodeCommand {
    val encoded: Encoded = encoder("HSTRLEN").key(key).data(field).result
  }

  private final class Hvals(key: Key) extends RedisDataSeqCommand[Value] with NodeCommand {
    val encoded: Encoded = encoder("HVALS").key(key).result
  }
}
