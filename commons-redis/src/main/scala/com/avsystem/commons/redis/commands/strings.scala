package com.avsystem.commons
package redis.commands

import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion}
import com.avsystem.commons.redis.CommandEncoder.CommandArg
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.commands.ReplyDecoders._

trait StringsApi extends ApiSubset {
  /** Executes [[http://redis.io/commands/append APPEND]] */
  def append(key: Key, value: Value): Result[Int] =
    execute(new Append(key, value))

  /** Executes [[http://redis.io/commands/bitcount BITCOUNT]] */
  def bitcount(key: Key, range: OptArg[(Int, Int)] = OptArg.Empty): Result[Long] =
    execute(new Bitcount(key, range.toOpt))

  /** Executes [[http://redis.io/commands/bitfield BITFIELD]] */
  def bitfield(key: Key, op: BitFieldOp): Result[Opt[Long]] =
    execute(new Bitfield(key, op.single).map(_.head))

  /** Executes [[http://redis.io/commands/bitfield BITFIELD]] */
  def bitfield(key: Key, op: BitFieldOp, ops: BitFieldOp*): Result[Seq[Opt[Long]]] =
    execute(new Bitfield(key, op +:: ops))

  /** Executes [[http://redis.io/commands/bitfield BITFIELD]] */
  def bitfield(key: Key, ops: Iterable[BitFieldOp]): Result[Seq[Opt[Long]]] =
    execute(new Bitfield(key, ops))

  /** Executes [[http://redis.io/commands/bitop BITOP]] */
  def bitop(multiOperation: MultiBitOp, destkey: Key, keys: Key*): Result[Int] =
    execute(new Bitop(multiOperation, destkey, keys))

  /** Executes [[http://redis.io/commands/bitop BITOP]] */
  def bitopNot(destkey: Key, key: Key): Result[Int] =
    execute(new Bitop(BitOp.Not, destkey, List(key)))

  /** Executes [[http://redis.io/commands/bitpos BITPOS]] */
  def bitpos(key: Key, bit: Boolean): Result[Long] =
    execute(new Bitpos(key, bit, Opt.Empty))

  /** Executes [[http://redis.io/commands/bitpos BITPOS]] */
  def bitpos(key: Key, bit: Boolean, start: Int): Result[Long] =
    execute(new Bitpos(key, bit, SemiRange(start).opt))

  /** Executes [[http://redis.io/commands/bitpos BITPOS]] */
  def bitpos(key: Key, bit: Boolean, start: Int, end: Int): Result[Long] =
    execute(new Bitpos(key, bit, SemiRange(start, end.opt).opt))

  /** Executes [[http://redis.io/commands/decr DECR]] */
  def decr(key: Key): Result[Long] =
    execute(new Decr(key))

  /** Executes [[http://redis.io/commands/decrby DECRBY]] */
  def decrby(key: Key, decrement: Long): Result[Long] =
    execute(new Decrby(key, decrement))

  /** Executes [[http://redis.io/commands/get GET]] */
  def get(key: Key): Result[Opt[Value]] =
    execute(new Get(key))

  /** Executes [[http://redis.io/commands/getbit GETBIT]] */
  def getbit(key: Key, offset: Int): Result[Boolean] =
    execute(new Getbit(key, offset))

  /** Executes [[http://redis.io/commands/getrange GETRANGE]] */
  def getrange(key: Key, start: Int = 0, end: Int = -1): Result[Value] =
    execute(new Getrange(key, start, end))

  /** Executes [[http://redis.io/commands/getset GETSET]] */
  def getset(key: Key, value: Value): Result[Opt[Value]] =
    execute(new Getset(key, value))

  /** Executes [[http://redis.io/commands/incr INCR]] */
  def incr(key: Key): Result[Long] =
    execute(new Incr(key))

  /** Executes [[http://redis.io/commands/incrby INCRBY]] */
  def incrby(key: Key, increment: Long): Result[Long] =
    execute(new Incrby(key, increment))

  /** Executes [[http://redis.io/commands/incrbyfloat INCRBYFLOAT]] */
  def incrbyfloat(key: Key, increment: Double): Result[Double] =
    execute(new Incrbyfloat(key, increment))

  /** Executes [[http://redis.io/commands/mget MGET]] */
  def mget(key: Key, keys: Key*): Result[Seq[Opt[Value]]] =
    execute(new Mget(key +:: keys))

  /** Executes [[http://redis.io/commands/mget MGET]]
    * or simply returns empty `Seq` when `keys` is empty, without sending the command to Redis */
  def mget(keys: Iterable[Key]): Result[Seq[Opt[Value]]] =
    execute(new Mget(keys))

  /** Executes [[http://redis.io/commands/mset MSET]] */
  def mset(keyValue: (Key, Value), keyValues: (Key, Value)*): Result[Unit] =
    execute(new Mset(keyValue +:: keyValues))

  /** Executes [[http://redis.io/commands/mset MSET]]
    * or does nothing when `keyValues` is empty, without sending the command to Redis */
  def mset(keyValues: Iterable[(Key, Value)]): Result[Unit] =
    execute(new Mset(keyValues))

  /** Executes [[http://redis.io/commands/msetnx MSETNX]] */
  def msetnx(keyValue: (Key, Value), keyValues: (Key, Value)*): Result[Boolean] =
    execute(new Msetnx(keyValue +:: keyValues))

  /** Executes [[http://redis.io/commands/msetnx MSETNX]]
    * or simply returns `true` when `keyValues` is empty, without sending the command to Redis */
  def msetnx(keyValues: Iterable[(Key, Value)]): Result[Boolean] =
    execute(new Msetnx(keyValues))

  /** Executes [[http://redis.io/commands/psetex PSETEX]] */
  def psetex(key: Key, milliseconds: Long, value: Value): Result[Unit] =
    execute(new Psetex(key, milliseconds, value))

  /** Executes [[http://redis.io/commands/set SET]] */
  def set(key: Key, value: Value, expiration: OptArg[Expiration] = OptArg.Empty, existence: OptArg[Boolean] = OptArg.Empty): Result[Boolean] =
    execute(new Set(key, value, expiration.toOpt, existence.toOpt))

  /** Executes [[http://redis.io/commands/setbit SETBIT]] */
  def setbit(key: Key, offset: Long, value: Boolean): Result[Boolean] =
    execute(new Setbit(key, offset, value))

  /** Executes [[http://redis.io/commands/setex SETEX]] */
  def setex(key: Key, seconds: Long, value: Value): Result[Unit] =
    execute(new Setex(key, seconds, value))

  /** Executes [[http://redis.io/commands/setnx SETNX]] */
  def setnx(key: Key, value: Value): Result[Boolean] =
    execute(new Setnx(key, value))

  /** Executes [[http://redis.io/commands/setrange SETRANGE]] */
  def setrange(key: Key, offset: Int, value: Value): Result[Int] =
    execute(new Setrange(key, offset, value))

  /** Executes [[http://redis.io/commands/strlen STRLEN]] */
  def strlen(key: Key): Result[Int] =
    execute(new Strlen(key))

  private final class Append(key: Key, value: Value) extends RedisIntCommand with NodeCommand {
    val encoded: Encoded = encoder("APPEND").key(key).data(value).result
  }

  private final class Bitcount(key: Key, range: Opt[(Int, Int)]) extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("BITCOUNT").key(key).optAdd(range).result
  }

  private final class Bitfield(key: Key, ops: Iterable[BitFieldOp])
    extends RedisSeqCommand[Opt[Long]](nullBulkOr(integerAsLong)) with NodeCommand {

    val encoded: Encoded = {
      import BitFieldOp._
      val enc = encoder("BITFIELD").key(key)
      def loop(it: Iterator[BitFieldOp], curOverflow: Overflow): Unit = if (it.hasNext) {
        def ensureOverflow(overflow: Overflow) =
          if (overflow != curOverflow) {
            enc.add("OVERFLOW").add(overflow)
          }
        it.next() match {
          case Get(field) =>
            enc.add("GET").add(field)
            loop(it, curOverflow)
          case Set(field, overflow, value) =>
            ensureOverflow(overflow)
            enc.add("SET").add(field).add(value)
            loop(it, overflow)
          case Incrby(field, overflow, increment) =>
            ensureOverflow(overflow)
            enc.add("INCRBY").add(field).add(increment)
            loop(it, overflow)
        }
      }
      loop(ops.iterator, Overflow.Wrap)
      enc.result
    }
  }

  private final class Bitop(bitop: BitOp, destkey: Key, keys: Seq[Key]) extends RedisIntCommand with NodeCommand {
    require(keys.nonEmpty, "BITOP requires at least one source key")
    require(bitop != BitOp.Not || keys.size == 1, "BITOP NOT requires exactly one source key")
    val encoded: Encoded = encoder("BITOP").add(bitop).key(destkey).keys(keys).result
  }

  private final class Bitpos(key: Key, bit: Boolean, range: Opt[SemiRange]) extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("BITPOS").key(key).add(bit).optAdd(range).result
  }

  private final class Decr(key: Key) extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("DECR").key(key).result
  }

  private final class Decrby(key: Key, decrement: Long) extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("DECRBY").key(key).add(decrement).result
  }

  private final class Get(key: Key) extends RedisOptDataCommand[Value] with NodeCommand {
    val encoded: Encoded = encoder("GET").key(key).result
  }

  private final class Getbit(key: Key, offset: Int) extends RedisBooleanCommand with NodeCommand {
    val encoded: Encoded = encoder("GETBIT").key(key).add(offset).result
  }

  private final class Getrange(key: Key, start: Int, end: Int) extends RedisDataCommand[Value] with NodeCommand {
    val encoded: Encoded = encoder("GETRANGE").key(key).add(start).add(end).result
  }

  private final class Getset(key: Key, value: Value) extends RedisOptDataCommand[Value] with NodeCommand {
    val encoded: Encoded = encoder("GETSET").key(key).data(value).result
  }

  private final class Incr(key: Key) extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("INCR").key(key).result
  }

  private final class Incrby(key: Key, increment: Long) extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("INCRBY").key(key).add(increment).result
  }

  private final class Incrbyfloat(key: Key, increment: Double) extends RedisDoubleCommand with NodeCommand {
    val encoded: Encoded = encoder("INCRBYFLOAT").key(key).add(increment).result
  }

  private final class Mget(keys: Iterable[Key]) extends RedisOptDataSeqCommand[Value] with NodeCommand {
    val encoded: Encoded = encoder("MGET").keys(keys).result
    override def immediateResult: Opt[Seq[Opt[Value]]] = whenEmpty(keys, Seq.empty)
  }

  private final class Mset(keyValues: Iterable[(Key, Value)]) extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("MSET").keyDatas(keyValues).result
    override def immediateResult: Opt[Unit] = whenEmpty(keyValues, ())
  }

  private final class Msetnx(keyValues: Iterable[(Key, Value)]) extends RedisBooleanCommand with NodeCommand {
    val encoded: Encoded = encoder("MSETNX").keyDatas(keyValues).result
    override def immediateResult: Opt[Boolean] = whenEmpty(keyValues, true)
  }

  private final class Psetex(key: Key, milliseconds: Long, value: Value) extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("PSETEX").key(key).add(milliseconds).data(value).result
  }

  private final class Set(key: Key, value: Value, expiration: Opt[Expiration], existence: Opt[Boolean])
    extends AbstractRedisCommand[Boolean](nullBulkOrSimpleOkAsBoolean) with NodeCommand {

    val encoded: Encoded = encoder("SET").key(key).data(value).optAdd(expiration)
      .optAdd(existence.map(v => if (v) "XX" else "NX")).result
  }

  private final class Setbit(key: Key, offset: Long, value: Boolean) extends RedisBooleanCommand with NodeCommand {
    val encoded: Encoded = encoder("SETBIT").key(key).add(offset).add(value).result
  }

  private final class Setex(key: Key, seconds: Long, value: Value) extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("SETEX").key(key).add(seconds).data(value).result
  }

  private final class Setnx(key: Key, value: Value) extends RedisBooleanCommand with NodeCommand {
    val encoded: Encoded = encoder("SETNX").key(key).data(value).result
  }

  private final class Setrange(key: Key, offset: Int, value: Value) extends RedisIntCommand with NodeCommand {
    val encoded: Encoded = encoder("SETRANGE").key(key).add(offset).data(value).result
  }

  private final class Strlen(key: Key) extends RedisIntCommand with NodeCommand {
    val encoded: Encoded = encoder("STRLEN").key(key).result
  }
}

case class BitFieldType(signed: Boolean, width: Int) {
  def at(offset: Long): BitField = BitField(this, offset, offsetInWidths = false)
  def atWidths(offset: Long): BitField = BitField(this, offset, offsetInWidths = true)
}
case class BitField(bfType: BitFieldType, offset: Long, offsetInWidths: Boolean) {

  import BitFieldOp._

  def get: BitFieldOp = Get(this)
  def set(value: Long): BitFieldMod = Set(this, Overflow.Wrap, value)
  def incrby(value: Long): BitFieldMod = Incrby(this, Overflow.Wrap, value)
}
object BitField {
  def signed(width: Int): BitFieldType = BitFieldType(signed = true, width)
  def unsigned(width: Int): BitFieldType = BitFieldType(signed = false, width)

  implicit val commandArg: CommandArg[BitField] = CommandArg {
    case (enc, BitField(BitFieldType(signed, width), offset, offsetInWidths)) =>
      enc.add((if (signed) "i" else "u") + width.toString).add((if (offsetInWidths) "#" else "") + offset.toString)
  }
}

sealed trait BitFieldOp {
  def field: BitField
}

sealed trait BitFieldMod extends BitFieldOp {

  import BitFieldOp.Overflow

  def overflow: Overflow
  def overflow(of: Overflow): BitFieldMod

  def overflowWrap: BitFieldMod = overflow(Overflow.Wrap)
  def overflowSat: BitFieldMod = overflow(Overflow.Sat)
  def overflowFail: BitFieldMod = overflow(Overflow.Fail)
}

object BitFieldOp {
  sealed abstract class Overflow(val name: String) extends NamedEnum
  object Overflow extends NamedEnumCompanion[Overflow] {
    case object Wrap extends Overflow("WRAP")
    case object Sat extends Overflow("SAT")
    case object Fail extends Overflow("FAIL")
    val values: List[Overflow] = caseObjects
  }

  case class Get(field: BitField) extends BitFieldOp
  case class Set(field: BitField, overflow: Overflow, value: Long) extends BitFieldMod {
    def overflow(of: Overflow): BitFieldMod = copy(overflow = of)
  }
  case class Incrby(field: BitField, overflow: Overflow, increment: Long) extends BitFieldMod {
    def overflow(of: Overflow): BitFieldMod = copy(overflow = of)
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
object SemiRange {
  implicit val SemiRangeArg: CommandArg[SemiRange] =
    CommandArg((enc, sa) => enc.add(sa.start).optAdd(sa.end))
}

sealed trait Expiration
object Expiration {
  case class Ex(seconds: Long) extends Expiration
  case class Px(milliseconds: Long) extends Expiration

  implicit val SetExpirationArg: CommandArg[Expiration] = CommandArg {
    case (ce, Ex(seconds)) => ce.add("EX").add(seconds)
    case (ce, Px(milliseconds)) => ce.add("PX").add(milliseconds)
  }
}
