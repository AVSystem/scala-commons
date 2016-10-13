package com.avsystem.commons
package redis.commands

import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion, Opt, OptArg}
import com.avsystem.commons.redis.CommandEncoder.CommandArg
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.commands.ReplyDecoders._

trait StringsApi extends ApiSubset {
  def append(key: Key, value: Value): Result[Int] =
    execute(new Append(key, value))
  def bitcount(key: Key, range: OptArg[(Int, Int)] = OptArg.Empty): Result[Long] =
    execute(new Bitcount(key, range.toOpt))
  def bitop(multiOperation: MultiBitOp, destkey: Key, keys: Key*): Result[Int] =
    execute(new Bitop(multiOperation, destkey, keys))
  def bitopNot(destkey: Key, key: Key): Result[Int] =
    execute(new Bitop(BitOp.Not, destkey, List(key)))
  def bitpos(key: Key, bit: Boolean): Result[Long] =
    execute(new Bitpos(key, bit, Opt.Empty))
  def bitpos(key: Key, bit: Boolean, start: Int): Result[Long] =
    execute(new Bitpos(key, bit, SemiRange(start).opt))
  def bitpos(key: Key, bit: Boolean, start: Int, end: Int): Result[Long] =
    execute(new Bitpos(key, bit, SemiRange(start, end.opt).opt))
  def decr(key: Key): Result[Long] =
    execute(new Decr(key))
  def decrby(key: Key, decrement: Long): Result[Long] =
    execute(new Decrby(key, decrement))
  def get(key: Key): Result[Opt[Value]] =
    execute(new Get(key))
  def getbit(key: Key, offset: Int): Result[Boolean] =
    execute(new Getbit(key, offset))
  def getrange(key: Key, start: Int = 0, end: Int = -1): Result[Value] =
    execute(new Getrange(key, start, end))
  def getset(key: Key, value: Value): Result[Opt[Value]] =
    execute(new Getset(key, value))
  def incr(key: Key): Result[Long] =
    execute(new Incr(key))
  def incrby(key: Key, increment: Long): Result[Long] =
    execute(new Incrby(key, increment))
  def incrbyfloat(key: Key, increment: Double): Result[Double] =
    execute(new Incrbyfloat(key, increment))
  def mget(key: Key, keys: Key*): Result[Seq[Opt[Value]]] =
    execute(new Mget(key +:: keys))
  def mget(keys: Iterable[Key]): Result[Seq[Opt[Value]]] =
    execute(new Mget(keys))
  def mset(keyValue: (Key, Value), keyValues: (Key, Value)*): Result[Unit] =
    execute(new Mset(keyValue +:: keyValues))
  def mset(keyValues: Iterable[(Key, Value)]): Result[Unit] =
    execute(new Mset(keyValues))
  def msetnx(keyValue: (Key, Value), keyValues: (Key, Value)*): Result[Boolean] =
    execute(new Msetnx(keyValue +:: keyValues))
  def msetnx(keyValues: Iterable[(Key, Value)]): Result[Boolean] =
    execute(new Msetnx(keyValues))
  def psetex(key: Key, milliseconds: Long, value: Value): Result[Unit] =
    execute(new Psetex(key, milliseconds, value))
  def set(key: Key, value: Value, expiration: OptArg[Expiration] = OptArg.Empty, existence: OptArg[Boolean] = OptArg.Empty): Result[Boolean] =
    execute(new Set(key, value, expiration.toOpt, existence.toOpt))
  def setbit(key: Key, offset: Long, value: Boolean): Result[Boolean] =
    execute(new Setbit(key, offset, value))
  def setex(key: Key, seconds: Long, value: Value): Result[Unit] =
    execute(new Setex(key, seconds, value))
  def setnx(key: Key, value: Value): Result[Boolean] =
    execute(new Setnx(key, value))
  def setrange(key: Key, offset: Int, value: Value): Result[Int] =
    execute(new Setrange(key, offset, value))
  def strlen(key: Key): Result[Int] =
    execute(new Strlen(key))

  private final class Append(key: Key, value: Value) extends RedisIntCommand with NodeCommand {
    val encoded = encoder("APPEND").key(key).data(value).result
  }

  private final class Bitcount(key: Key, range: Opt[(Int, Int)]) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("BITCOUNT").key(key).add(range).result
  }

  private final class Bitop(bitop: BitOp, destkey: Key, keys: Seq[Key]) extends RedisIntCommand with NodeCommand {
    require(keys.nonEmpty, "BITOP requires at least one source key")
    require(bitop != BitOp.Not || keys.size == 1, "BITOP NOT requires exactly one source key")
    val encoded = encoder("BITOP").add(bitop).key(destkey).keys(keys).result
  }

  private final class Bitpos(key: Key, bit: Boolean, range: Opt[SemiRange]) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("BITPOS").key(key).add(bit).add(range.map(sr => (sr.start, sr.end))).result
  }

  private final class Decr(key: Key) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("DECR").key(key).result
  }

  private final class Decrby(key: Key, decrement: Long) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("DECRBY").key(key).add(decrement).result
  }

  private final class Get(key: Key) extends RedisOptDataCommand[Value] with NodeCommand {
    val encoded = encoder("GET").key(key).result
  }

  private final class Getbit(key: Key, offset: Int) extends RedisBooleanCommand with NodeCommand {
    val encoded = encoder("GETBIT").key(key).add(offset).result
  }

  private final class Getrange(key: Key, start: Int, end: Int) extends RedisDataCommand[Value] with NodeCommand {
    val encoded = encoder("GETRANGE").key(key).add(start).add(end).result
  }

  private final class Getset(key: Key, value: Value) extends RedisOptDataCommand[Value] with NodeCommand {
    val encoded = encoder("GETSET").key(key).data(value).result
  }

  private final class Incr(key: Key) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("INCR").key(key).result
  }

  private final class Incrby(key: Key, increment: Long) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("INCRBY").key(key).add(increment).result
  }

  private final class Incrbyfloat(key: Key, increment: Double) extends RedisDoubleCommand with NodeCommand {
    val encoded = encoder("INCRBYFLOAT").key(key).add(increment).result
  }

  private final class Mget(keys: Iterable[Key]) extends RedisOptDataSeqCommand[Value] with NodeCommand {
    requireNonEmpty(keys, "keys")
    val encoded = encoder("MGET").keys(keys).result
  }

  private final class Mset(keyValues: Iterable[(Key, Value)]) extends RedisUnitCommand with NodeCommand {
    requireNonEmpty(keyValues, "keyValues")
    val encoded = encoder("MSET").keyDatas(keyValues).result
  }

  private final class Msetnx(keyValues: Iterable[(Key, Value)]) extends RedisBooleanCommand with NodeCommand {
    requireNonEmpty(keyValues, "keyValues")
    val encoded = encoder("MSETNX").keyDatas(keyValues).result
  }

  private final class Psetex(key: Key, milliseconds: Long, value: Value) extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("PSETEX").key(key).add(milliseconds).data(value).result
  }

  private final class Set(key: Key, value: Value, expiration: Opt[Expiration], existence: Opt[Boolean])
    extends AbstractRedisCommand[Boolean](nullBulkOrSimpleOkBoolean) with NodeCommand {

    val encoded = encoder("SET").key(key).data(value).add(expiration)
      .add(existence.map(v => if (v) "XX" else "NX")).result
  }

  private final class Setbit(key: Key, offset: Long, value: Boolean) extends RedisBooleanCommand with NodeCommand {
    val encoded = encoder("SETBIT").key(key).add(offset).add(value).result
  }

  private final class Setex(key: Key, seconds: Long, value: Value) extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("SETEX").key(key).add(seconds).data(value).result
  }

  private final class Setnx(key: Key, value: Value) extends RedisBooleanCommand with NodeCommand {
    val encoded = encoder("SETNX").key(key).data(value).result
  }

  private final class Setrange(key: Key, offset: Int, value: Value) extends RedisIntCommand with NodeCommand {
    val encoded = encoder("SETRANGE").key(key).add(offset).data(value).result
  }

  private final class Strlen(key: Key) extends RedisIntCommand with NodeCommand {
    val encoded = encoder("STRLEN").key(key).result
  }
}

sealed abstract class BitOp(val name: String) extends NamedEnum
sealed abstract class MultiBitOp(name: String) extends BitOp(name)
object BitOp extends NamedEnumCompanion[BitOp] {
  case object And extends MultiBitOp("AND")
  case object Or extends MultiBitOp("OR")
  case object Xor extends MultiBitOp("XOR")
  case object Not extends BitOp("NOT")

  val values: List[BitOp] = caseObjects
}

case class SemiRange(start: Int, end: Opt[Int] = Opt.Empty)

sealed trait Expiration
object Expiration {
  case class Ex(seconds: Long) extends Expiration
  case class Px(milliseconds: Long) extends Expiration

  implicit val SetExpirationArg: CommandArg[Expiration] = CommandArg {
    case (ce, Ex(seconds)) => ce.add("EX").add(seconds)
    case (ce, Px(milliseconds)) => ce.add("PX").add(milliseconds)
  }
}
