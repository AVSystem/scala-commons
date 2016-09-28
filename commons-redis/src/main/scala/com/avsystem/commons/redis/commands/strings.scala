package com.avsystem.commons
package redis.commands

import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion, Opt}
import com.avsystem.commons.redis.CommandEncoder.CommandArg
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.protocol.{NullBulkStringMsg, SimpleStringStr}

trait StringsApi extends ApiSubset {
  def append(key: Key, value: Value): Result[Long] =
    execute(Append(key, value))
  def bitcount(key: Key, range: Opt[(Long, Long)] = Opt.Empty): Result[Long] =
    execute(Bitcount(key, range))
  def bitop(multiOperation: MultiBitOperation, destkey: Key, keys: Seq[Key]): Result[Long] =
    execute(Bitop(multiOperation, destkey, keys))
  def bitopNot(destkey: Key, key: Key): Result[Long] =
    execute(Bitop(BitOperation.Not, destkey, List(key)))
  def bitpos(key: Key, bit: Boolean, range: Opt[SemiRange] = Opt.Empty): Result[Long] =
    execute(Bitpos(key, bit, range))
  def decr(key: Key): Result[Long] =
    execute(Decr(key))
  def decrby(key: Key, decrement: Long): Result[Long] =
    execute(Decrby(key, decrement))
  def get(key: Key): Result[Opt[Value]] =
    execute(Get(key))
  def getbit(key: Key, offset: Long): Result[Boolean] =
    execute(Getbit(key, offset))
  def getrange(key: Key, start: Long, end: Long): Result[Value] =
    execute(Getrange(key, start, end))
  def getset(key: Key, value: Value): Result[Opt[Value]] =
    execute(Getset(key, value))
  def incr(key: Key): Result[Long] =
    execute(Incr(key))
  def incrby(key: Key, increment: Long): Result[Long] =
    execute(Incrby(key, increment))
  def incrbyfloat(key: Key, increment: Double): Result[Double] =
    execute(Incrbyfloat(key, increment))
  def mget(keys: Seq[Key]): Result[Seq[Opt[Value]]] =
    execute(Mget(keys))
  def mset(keyValues: Seq[(Key, Value)]): Result[Unit] =
    execute(Mset(keyValues))
  def msetnx(keyValues: Seq[(Key, Value)]): Result[Boolean] =
    execute(Msetnx(keyValues))
  def psetex(key: Key, milliseconds: Long, value: Value): Result[Unit] =
    execute(Psetex(key, milliseconds, value))
  def set(key: Key, value: Value, expiration: Opt[SetExpiration] = Opt.Empty, existence: Opt[Boolean] = Opt.Empty): Result[Boolean] =
    execute(Set(key, value, expiration, existence))
  def setbit(key: Key, offset: Long, value: Value): Result[Boolean] =
    execute(Setbit(key, offset, value))
  def setex(key: Key, seconds: Long, value: Value): Result[Unit] =
    execute(Setex(key, seconds, value))
  def setnx(key: Key, value: Value): Result[Boolean] =
    execute(Setnx(key, value))
  def setrange(key: Key, offset: Long, value: Value): Result[Long] =
    execute(Setrange(key, offset, value))
  def strlen(key: Key): Result[Long] =
    execute(Strlen(key))

  private case class Append(key: Key, value: Value) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("APPEND").key(key).value(value).result
  }

  private case class Bitcount(key: Key, range: Opt[(Long, Long)]) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("BITCOUNT").add(range).result
  }

  private case class Bitop(bitop: BitOperation, destkey: Key, keys: Seq[Key]) extends RedisLongCommand with NodeCommand {
    require(keys.nonEmpty, "BITOP requires at least one source key")
    require(bitop != BitOperation.Not || keys.size == 1, "BITOP NOT requires exactly one source key")
    val encoded = encoder("BITOP").add(bitop).key(destkey).keys(keys).result
  }

  private case class Bitpos(key: Key, bit: Boolean, range: Opt[SemiRange]) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("BITPOS").key(key).add(bit).add(range.map(sr => (sr.start, sr.end))).result
  }

  private case class Decr(key: Key) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("DECR").key(key).result
  }

  private case class Decrby(key: Key, decrement: Long) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("DECRBY").key(key).add(decrement).result
  }

  private case class Get(key: Key) extends RedisOptDataCommand[Value] with HasValueCodec with NodeCommand {
    val encoded = encoder("GET").key(key).result
  }

  private case class Getbit(key: Key, offset: Long) extends RedisBooleanCommand with NodeCommand {
    val encoded = encoder("GETBIT").key(key).add(offset).result
  }

  private case class Getrange(key: Key, start: Long, end: Long) extends RedisDataCommand[Value] with HasValueCodec with NodeCommand {
    val encoded = encoder("GETRANGE").key(key).add(start).add(end).result
  }

  private case class Getset(key: Key, value: Value) extends RedisOptDataCommand[Value] with HasValueCodec with NodeCommand {
    val encoded = encoder("GETSET").key(key).value(value).result
  }

  private case class Incr(key: Key) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("INCR").key(key).result
  }

  private case class Incrby(key: Key, increment: Long) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("INCRBY").key(key).add(increment).result
  }

  private case class Incrbyfloat(key: Key, increment: Double) extends RedisDoubleCommand with NodeCommand {
    val encoded = encoder("INCRBYFLOAT").key(key).add(increment).result
  }

  private case class Mget(keys: Seq[Key]) extends RedisOptDataSeqCommand[Value] with HasValueCodec with NodeCommand {
    require(keys.nonEmpty, "MGET requires at least one key")
    val encoded = encoder("MGET").keys(keys).result
  }

  private case class Mset(keyValues: Seq[(Key, Value)]) extends RedisUnitCommand with NodeCommand {
    require(keyValues.nonEmpty, "MSET requires at least one key-value pair")
    val encoded = encoder("MSET").keyValues(keyValues).result
  }

  private case class Msetnx(keyValues: Seq[(Key, Value)]) extends RedisBooleanCommand with NodeCommand {
    require(keyValues.nonEmpty, "MSETNX requires at least one key-value pair")
    val encoded = encoder("MSETNX").keyValues(keyValues).result
  }

  private case class Psetex(key: Key, milliseconds: Long, value: Value) extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("PSETEX").key(key).add(milliseconds).value(value).result
  }

  private case class Set(key: Key, value: Value, expiration: Opt[SetExpiration], existence: Opt[Boolean])
    extends RedisCommand[Boolean] with NodeCommand {

    val encoded = encoder("SET").key(key).value(value).add(expiration)
      .add(existence.map(v => if (v) "XX" else "NX")).result

    def decodeExpected = {
      case SimpleStringStr("OK") => true
      case NullBulkStringMsg => false
    }
  }

  private case class Setbit(key: Key, offset: Long, value: Value) extends RedisBooleanCommand with NodeCommand {
    val encoded = encoder("SETBIT").key(key).add(offset).value(value).result
  }

  private case class Setex(key: Key, seconds: Long, value: Value) extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("SETEX").key(key).add(seconds).value(value).result
  }

  private case class Setnx(key: Key, value: Value) extends RedisBooleanCommand with NodeCommand {
    val encoded = encoder("SETNX").key(key).value(value).result
  }

  private case class Setrange(key: Key, offset: Long, value: Value) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("SETRANGE").key(key).add(offset).value(value).result
  }

  private case class Strlen(key: Key) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("STRLEN").key(key).result
  }
}

sealed abstract class BitOperation(val name: String) extends NamedEnum
sealed abstract class MultiBitOperation(name: String) extends BitOperation(name)
object BitOperation extends NamedEnumCompanion[BitOperation] {
  case object And extends MultiBitOperation("AND")
  case object Or extends MultiBitOperation("OR")
  case object Xor extends MultiBitOperation("XOR")
  case object Not extends BitOperation("NOT")

  val values: List[BitOperation] = caseObjects
}

case class SemiRange(start: Long, end: Opt[Long] = Opt.Empty)

sealed trait SetExpiration
object SetExpiration {
  case class Ex(seconds: Long) extends SetExpiration
  case class Px(milliseconds: Long) extends SetExpiration

  implicit val SetExpirationArg: CommandArg[SetExpiration] = CommandArg {
    case (ce, Ex(seconds)) => ce.add("EX").add(seconds)
    case (ce, Px(milliseconds)) => ce.add("PX").add(milliseconds)
  }
}
