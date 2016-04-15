package com.avsystem.commons
package redis.commands

import akka.util.ByteString
import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion, Opt}
import com.avsystem.commons.redis.CommandEncoder.CommandArg
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.protocol.{NullBulkStringMsg, SimpleStringStr}

trait StringsApi[+F[_]] extends ApiSubset[F] {
  def append(key: ByteString, value: ByteString): F[Long] =
    mapper(Append(key, value))
  def bitcount(key: ByteString, range: Opt[(Long, Long)] = Opt.Empty): F[Long] =
    mapper(Bitcount(key, range))
  def bitop(multiOperation: MultiBitOperation, destkey: ByteString, keys: Seq[ByteString]): F[Long] =
    mapper(Bitop(multiOperation, destkey, keys))
  def bitopNot(destkey: ByteString, key: ByteString): F[Long] =
    mapper(Bitop(BitOperation.Not, destkey, List(key)))
  def bitpos(key: ByteString, bit: Boolean, range: Opt[SemiRange] = Opt.Empty): F[Long] =
    mapper(Bitpos(key, bit, range))
  def decr(key: ByteString): F[Long] =
    mapper(Decr(key))
  def decrby(key: ByteString, decrement: Long): F[Long] =
    mapper(Decrby(key, decrement))
  def get(key: ByteString): F[Opt[ByteString]] =
    mapper(Get(key))
  def getbit(key: ByteString, offset: Long): F[Boolean] =
    mapper(Getbit(key, offset))
  def getrange(key: ByteString, start: Long, end: Long): F[ByteString] =
    mapper(Getrange(key, start, end))
  def getset(key: ByteString, value: ByteString): F[Opt[ByteString]] =
    mapper(Getset(key, value))
  def incr(key: ByteString): F[Long] =
    mapper(Incr(key))
  def incrby(key: ByteString, increment: Long): F[Long] =
    mapper(Incrby(key, increment))
  def incrbyfloat(key: ByteString, increment: Double): F[Double] =
    mapper(Incrbyfloat(key, increment))
  def mget(keys: Seq[ByteString]): F[Seq[Opt[ByteString]]] =
    mapper(Mget(keys))
  def mset(keyValues: Seq[(ByteString, ByteString)]): F[Unit] =
    mapper(Mset(keyValues))
  def msetnx(keyValues: Seq[(ByteString, ByteString)]): F[Boolean] =
    mapper(Msetnx(keyValues))
  def psetex(key: ByteString, milliseconds: Long, value: ByteString): F[Unit] =
    mapper(Psetex(key, milliseconds, value))
  def set(key: ByteString, value: ByteString, expiration: Opt[SetExpiration] = Opt.Empty, existence: Opt[Boolean] = Opt.Empty): F[Boolean] =
    mapper(Set(key, value, expiration, existence))
  def setbit(key: ByteString, offset: Long, value: ByteString): F[Boolean] =
    mapper(Setbit(key, offset, value))
  def setex(key: ByteString, seconds: Long, value: ByteString): F[Unit] =
    mapper(Setex(key, seconds, value))
  def setnx(key: ByteString, value: ByteString): F[Boolean] =
    mapper(Setnx(key, value))
  def setrange(key: ByteString, offset: Long, value: ByteString): F[Long] =
    mapper(Setrange(key, offset, value))
  def strlen(key: ByteString): F[Long] =
    mapper(Strlen(key))
}

case class Append(key: ByteString, value: ByteString) extends RedisLongCommand with SimpleSingleKeyed {
  def encode = encoder("APPEND").add(key).add(value).result
}

case class Bitcount(key: ByteString, range: Opt[(Long, Long)]) extends RedisLongCommand with SimpleSingleKeyed {
  def encode = encoder("BITCOUNT").add(range).result
}

case class Bitop(bitop: BitOperation, destkey: ByteString, keys: Seq[ByteString]) extends RedisLongCommand {
  require(keys.nonEmpty, "BITOP requires at least one source key")
  require(bitop != BitOperation.Not || keys.size == 1, "BITOP NOT requires exactly one source key")
  def encode = encoder("BITOP").add(bitop).add(destkey).add(keys).result
  def isKey(idx: Int) = idx >= 2
}

case class Bitpos(key: ByteString, bit: Boolean, range: Opt[SemiRange]) extends RedisLongCommand with SimpleSingleKeyed {
  def encode = encoder("BITPOS").add(key).add(bit).add(range.map(sr => (sr.start, sr.end))).result
}

case class Decr(key: ByteString) extends RedisLongCommand with SimpleSingleKeyed {
  def encode = encoder("DECR").add(key).result
}

case class Decrby(key: ByteString, decrement: Long) extends RedisLongCommand with SimpleSingleKeyed {
  def encode = encoder("DECRBY").add(key).add(decrement).result
}

case class Get(key: ByteString) extends RedisOptBinaryCommand with SimpleSingleKeyed {
  def encode = encoder("GET").add(key).result
}

case class Getbit(key: ByteString, offset: Long) extends RedisBooleanCommand with SimpleSingleKeyed {
  def encode = encoder("GETBIT").add(key).add(offset).result
}

case class Getrange(key: ByteString, start: Long, end: Long) extends RedisBinaryCommand with SimpleSingleKeyed {
  def encode = encoder("GETRANGE").add(key).add(start).add(end).result
}

case class Getset(key: ByteString, value: ByteString) extends RedisOptBinaryCommand with SimpleSingleKeyed {
  def encode = encoder("GETSET").add(key).add(value).result
}

case class Incr(key: ByteString) extends RedisLongCommand with SimpleSingleKeyed {
  def encode = encoder("INCR").add(key).result
}

case class Incrby(key: ByteString, increment: Long) extends RedisLongCommand with SimpleSingleKeyed {
  def encode = encoder("INCRBY").add(key).add(increment).result
}

case class Incrbyfloat(key: ByteString, increment: Double) extends RedisDoubleCommand with SimpleSingleKeyed {
  def encode = encoder("INCRBYFLOAT").add(key).add(increment).result
}

case class Mget(keys: Seq[ByteString]) extends RedisOptBinarySeqCommand {
  require(keys.nonEmpty, "MGET requires at least one key")
  def encode = encoder("MGET").add(keys).result
  def isKey(idx: Int) = idx >= 1
}

case class Mset(keyValues: Seq[(ByteString, ByteString)]) extends RedisUnitCommand {
  require(keyValues.nonEmpty, "MSET requires at least one key-value pair")
  def encode = encoder("MSET").add(keyValues).result
  def isKey(idx: Int) = (idx % 2) == 1
}

case class Msetnx(keyValues: Seq[(ByteString, ByteString)]) extends RedisBooleanCommand {
  require(keyValues.nonEmpty, "MSETNX requires at least one key-value pair")
  def encode = encoder("MSETNX").add(keyValues).result
  def isKey(idx: Int) = (idx % 2) == 1
}

case class Psetex(key: ByteString, milliseconds: Long, value: ByteString) extends RedisUnitCommand with SimpleSingleKeyed {
  def encode = encoder("PSETEX").add(key).add(milliseconds).add(value).result
}

case class Set(key: ByteString, value: ByteString, expiration: Opt[SetExpiration], existence: Opt[Boolean])
  extends RedisCommand[Boolean] with SimpleSingleKeyed {

  def encode = encoder("SET").add(key).add(value).add(expiration)
    .add(existence.map(v => if (v) "XX" else "NX")).result

  def decodeExpected = {
    case SimpleStringStr("OK") => true
    case NullBulkStringMsg => false
  }
}

case class Setbit(key: ByteString, offset: Long, value: ByteString) extends RedisBooleanCommand with SimpleSingleKeyed {
  def encode = encoder("SETBIT").add(key).add(offset).add(value).result
}

case class Setex(key: ByteString, seconds: Long, value: ByteString) extends RedisUnitCommand with SimpleSingleKeyed {
  def encode = encoder("SETEX").add(key).add(seconds).add(value).result
}

case class Setnx(key: ByteString, value: ByteString) extends RedisBooleanCommand with SimpleSingleKeyed {
  def encode = encoder("SETNX").add(key).add(value).result
}

case class Setrange(key: ByteString, offset: Long, value: ByteString) extends RedisLongCommand with SimpleSingleKeyed {
  def encode = encoder("SETRANGE").add(key).add(offset).add(value).result
}

case class Strlen(key: ByteString) extends RedisLongCommand with SimpleSingleKeyed {
  def encode = encoder("STRLEN").add(key).result
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
