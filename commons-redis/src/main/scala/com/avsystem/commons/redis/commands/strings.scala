package com.avsystem.commons
package redis.commands

import akka.util.ByteString
import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion, Opt}
import com.avsystem.commons.redis.CommandEncoder.CommandArg
import com.avsystem.commons.redis.Scope.Cluster
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.protocol.{NullBulkStringMsg, SimpleStringStr}

trait StringsApi extends ClusteredApiSubset {
  def append(key: ByteString, value: ByteString): Result[Long, Cluster] =
    execute(Append(key, value))
  def bitcount(key: ByteString, range: Opt[(Long, Long)] = Opt.Empty): Result[Long, Cluster] =
    execute(Bitcount(key, range))
  def bitop(multiOperation: MultiBitOperation, destkey: ByteString, keys: Seq[ByteString]): Result[Long, Cluster] =
    execute(Bitop(multiOperation, destkey, keys))
  def bitopNot(destkey: ByteString, key: ByteString): Result[Long, Cluster] =
    execute(Bitop(BitOperation.Not, destkey, List(key)))
  def bitpos(key: ByteString, bit: Boolean, range: Opt[SemiRange] = Opt.Empty): Result[Long, Cluster] =
    execute(Bitpos(key, bit, range))
  def decr(key: ByteString): Result[Long, Cluster] =
    execute(Decr(key))
  def decrby(key: ByteString, decrement: Long): Result[Long, Cluster] =
    execute(Decrby(key, decrement))
  def get(key: ByteString): Result[Opt[ByteString], Cluster] =
    execute(Get(key))
  def getbit(key: ByteString, offset: Long): Result[Boolean, Cluster] =
    execute(Getbit(key, offset))
  def getrange(key: ByteString, start: Long, end: Long): Result[ByteString, Cluster] =
    execute(Getrange(key, start, end))
  def getset(key: ByteString, value: ByteString): Result[Opt[ByteString], Cluster] =
    execute(Getset(key, value))
  def incr(key: ByteString): Result[Long, Cluster] =
    execute(Incr(key))
  def incrby(key: ByteString, increment: Long): Result[Long, Cluster] =
    execute(Incrby(key, increment))
  def incrbyfloat(key: ByteString, increment: Double): Result[Double, Cluster] =
    execute(Incrbyfloat(key, increment))
  def mget(keys: Seq[ByteString]): Result[Seq[Opt[ByteString]], Cluster] =
    execute(Mget(keys))
  def mset(keyValues: Seq[(ByteString, ByteString)]): Result[Unit, Cluster] =
    execute(Mset(keyValues))
  def msetnx(keyValues: Seq[(ByteString, ByteString)]): Result[Boolean, Cluster] =
    execute(Msetnx(keyValues))
  def psetex(key: ByteString, milliseconds: Long, value: ByteString): Result[Unit, Cluster] =
    execute(Psetex(key, milliseconds, value))
  def set(key: ByteString, value: ByteString, expiration: Opt[SetExpiration] = Opt.Empty, existence: Opt[Boolean] = Opt.Empty): Result[Boolean, Cluster] =
    execute(Set(key, value, expiration, existence))
  def setbit(key: ByteString, offset: Long, value: ByteString): Result[Boolean, Cluster] =
    execute(Setbit(key, offset, value))
  def setex(key: ByteString, seconds: Long, value: ByteString): Result[Unit, Cluster] =
    execute(Setex(key, seconds, value))
  def setnx(key: ByteString, value: ByteString): Result[Boolean, Cluster] =
    execute(Setnx(key, value))
  def setrange(key: ByteString, offset: Long, value: ByteString): Result[Long, Cluster] =
    execute(Setrange(key, offset, value))
  def strlen(key: ByteString): Result[Long, Cluster] =
    execute(Strlen(key))
}

case class Append(key: ByteString, value: ByteString) extends RedisLongCommand[Cluster] {
  val encoded = encoder("APPEND").key(key).add(value).result
}

case class Bitcount(key: ByteString, range: Opt[(Long, Long)]) extends RedisLongCommand[Cluster] {
  val encoded = encoder("BITCOUNT").add(range).result
}

case class Bitop(bitop: BitOperation, destkey: ByteString, keys: Seq[ByteString]) extends RedisLongCommand[Cluster] {
  require(keys.nonEmpty, "BITOP requires at least one source key")
  require(bitop != BitOperation.Not || keys.size == 1, "BITOP NOT requires exactly one source key")
  val encoded = encoder("BITOP").add(bitop).key(destkey).keys(keys).result
}

case class Bitpos(key: ByteString, bit: Boolean, range: Opt[SemiRange]) extends RedisLongCommand[Cluster] {
  val encoded = encoder("BITPOS").key(key).add(bit).add(range.map(sr => (sr.start, sr.end))).result
}

case class Decr(key: ByteString) extends RedisLongCommand[Cluster] {
  val encoded = encoder("DECR").key(key).result
}

case class Decrby(key: ByteString, decrement: Long) extends RedisLongCommand[Cluster] {
  val encoded = encoder("DECRBY").key(key).add(decrement).result
}

case class Get(key: ByteString) extends RedisOptBinaryCommand[Cluster] {
  val encoded = encoder("GET").key(key).result
}

case class Getbit(key: ByteString, offset: Long) extends RedisBooleanCommand[Cluster] {
  val encoded = encoder("GETBIT").key(key).add(offset).result
}

case class Getrange(key: ByteString, start: Long, end: Long) extends RedisBinaryCommand[Cluster] {
  val encoded = encoder("GETRANGE").key(key).add(start).add(end).result
}

case class Getset(key: ByteString, value: ByteString) extends RedisOptBinaryCommand[Cluster] {
  val encoded = encoder("GETSET").key(key).add(value).result
}

case class Incr(key: ByteString) extends RedisLongCommand[Cluster] {
  val encoded = encoder("INCR").key(key).result
}

case class Incrby(key: ByteString, increment: Long) extends RedisLongCommand[Cluster] {
  val encoded = encoder("INCRBY").key(key).add(increment).result
}

case class Incrbyfloat(key: ByteString, increment: Double) extends RedisDoubleCommand[Cluster] {
  val encoded = encoder("INCRBYFLOAT").key(key).add(increment).result
}

case class Mget(keys: Seq[ByteString]) extends RedisOptBinarySeqCommand[Cluster] {
  require(keys.nonEmpty, "MGET requires at least one key")
  val encoded = encoder("MGET").keys(keys).result
}

case class Mset(keyValues: Seq[(ByteString, ByteString)]) extends RedisUnitCommand[Cluster] {
  require(keyValues.nonEmpty, "MSET requires at least one key-value pair")
  val encoded = encoder("MSET").keyValues(keyValues).result
}

case class Msetnx(keyValues: Seq[(ByteString, ByteString)]) extends RedisBooleanCommand[Cluster] {
  require(keyValues.nonEmpty, "MSETNX requires at least one key-value pair")
  val encoded = encoder("MSETNX").keyValues(keyValues).result
}

case class Psetex(key: ByteString, milliseconds: Long, value: ByteString) extends RedisUnitCommand[Cluster] {
  val encoded = encoder("PSETEX").key(key).add(milliseconds).add(value).result
}

case class Set(key: ByteString, value: ByteString, expiration: Opt[SetExpiration], existence: Opt[Boolean])
  extends RedisCommand[Boolean, Cluster] {

  val encoded = encoder("SET").key(key).add(value).add(expiration)
    .add(existence.map(v => if (v) "XX" else "NX")).result

  def decodeExpected = {
    case SimpleStringStr("OK") => true
    case NullBulkStringMsg => false
  }
}

case class Setbit(key: ByteString, offset: Long, value: ByteString) extends RedisBooleanCommand[Cluster] {
  val encoded = encoder("SETBIT").key(key).add(offset).add(value).result
}

case class Setex(key: ByteString, seconds: Long, value: ByteString) extends RedisUnitCommand[Cluster] {
  val encoded = encoder("SETEX").key(key).add(seconds).add(value).result
}

case class Setnx(key: ByteString, value: ByteString) extends RedisBooleanCommand[Cluster] {
  val encoded = encoder("SETNX").key(key).add(value).result
}

case class Setrange(key: ByteString, offset: Long, value: ByteString) extends RedisLongCommand[Cluster] {
  val encoded = encoder("SETRANGE").key(key).add(offset).add(value).result
}

case class Strlen(key: ByteString) extends RedisLongCommand[Cluster] {
  val encoded = encoder("STRLEN").key(key).result
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
