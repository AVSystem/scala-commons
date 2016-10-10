package com.avsystem.commons
package redis

import akka.util.ByteString
import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion, Opt}
import com.avsystem.commons.redis.CommandEncoder.CommandArg
import com.avsystem.commons.redis.RedisBatch.Index
import com.avsystem.commons.redis.commands.{Cursor, ReplyDecoder}
import com.avsystem.commons.redis.exception.{ErrorReplyException, UnexpectedReplyException}
import com.avsystem.commons.redis.protocol._

import scala.collection.mutable.ArrayBuffer

trait RedisCommand[+A] extends AtomicBatch[A] with RawCommand {
  protected def decodeExpected: ReplyDecoder[A]

  protected def decode(replyMsg: RedisReply): A = replyMsg match {
    case validReply: ValidRedisMsg =>
      decodeExpected.applyOrElse(validReply, (r: ValidRedisMsg) => throw new UnexpectedReplyException(r.toString))
    case err: ErrorMsg =>
      throw new ErrorReplyException(err)
    case error: FailureReply =>
      throw error.exception
    case _ =>
      throw new UnexpectedReplyException(replyMsg.toString)
  }

  def decodeReplies(replies: Int => RedisReply, idx: Index, inTransaction: Boolean) =
    decode(replies(idx.inc()))

  override def toString =
    encoded.elements.iterator.map(bs => RedisMsg.escape(bs.string)).mkString(" ")
}

abstract class AbstractRedisCommand[A](protected val decodeExpected: ReplyDecoder[A])
  extends RedisCommand[A]

final class CommandEncoder(private val buffer: ArrayBuffer[BulkStringMsg]) extends AnyVal {
  def result: ArrayMsg[BulkStringMsg] = ArrayMsg(buffer)

  private def fluent(code: => Any): CommandEncoder = {
    code
    this
  }

  def key[K: RedisDataCodec](k: K) = fluent(buffer += CommandKeyMsg(RedisDataCodec.write(k)))
  def keys[K: RedisDataCodec](keys: TraversableOnce[K]) = fluent(keys.foreach(key[K]))
  def data[V: RedisDataCodec](v: V) = fluent(buffer += BulkStringMsg(RedisDataCodec.write(v)))
  def datas[V: RedisDataCodec](values: TraversableOnce[V]) = fluent(values.foreach(data[V]))
  def add[T: CommandArg](value: T): CommandEncoder = fluent(CommandArg.add(this, value))
  def addFlag(flag: String, value: Boolean): CommandEncoder = if (value) add(flag) else this
  def optAdd[T: CommandArg](flag: String, value: Opt[T]) = fluent(value.foreach(t => add(flag).add(t)))
  def optKey[K: RedisDataCodec](flag: String, value: Opt[K]) = fluent(value.foreach(t => add(flag).key(t)))
  def optData[V: RedisDataCodec](flag: String, value: Opt[V]) = fluent(value.foreach(t => add(flag).data(t)))
  def keyDatas[K: RedisDataCodec, V: RedisDataCodec](keyDatas: TraversableOnce[(K, V)]) =
    fluent(keyDatas.foreach({ case (k, v) => key(k).data(v) }))
  def dataPairs[K: RedisDataCodec, V: RedisDataCodec](dataPairs: TraversableOnce[(K, V)]) =
    fluent(dataPairs.foreach({ case (k, v) => data(k).data(v) }))
  def argDataPairs[T: CommandArg, V: RedisDataCodec](argDataPairs: TraversableOnce[(T, V)]) =
    fluent(argDataPairs.foreach({ case (a, v) => add(a).data(v) }))
}

object CommandEncoder {
  case class CommandArg[-T](add: (CommandEncoder, T) => Any) extends AnyVal
  object CommandArg {
    def add[T](ce: CommandEncoder, value: T)(implicit ca: CommandArg[T]): Unit =
      ca.add(ce, value)

    implicit val ByteStringArg: CommandArg[ByteString] = CommandArg((ce, v) => ce.buffer += BulkStringMsg(v))
    implicit val BooleanArg: CommandArg[Boolean] = CommandArg((ce, v) => ce.add(if (v) 1 else 0))
    implicit val StringArg: CommandArg[String] = CommandArg((ce, v) => ce.add(ByteString(v)))
    implicit val IntArg: CommandArg[Int] = CommandArg((ce, v) => ce.add(v.toString))
    implicit val LongArg: CommandArg[Long] = CommandArg((ce, v) => ce.add(v.toString))
    implicit val DoubleArg: CommandArg[Double] = CommandArg((ce, v) => ce.add(v.toString))
    implicit val NamedEnumArg: CommandArg[NamedEnum] = CommandArg((ce, v) => ce.add(v.name))
    implicit def CollArg[T: CommandArg]: CommandArg[TraversableOnce[T]] =
      CommandArg((ce, v) => v.foreach(t => ce.add(t)))
    implicit def OptArg[T: CommandArg]: CommandArg[Opt[T]] =
      CommandArg((ce, v) => v.foreach(ce.add(_)))
    implicit def PairArg[A: CommandArg, B: CommandArg]: CommandArg[(A, B)] =
      CommandArg {
        case (ce, (a, b)) =>
          ce.add(a)
          ce.add(b)
      }
  }
}

import com.avsystem.commons.redis.commands.ReplyDecoders._

abstract class RedisDataCommand[A: RedisDataCodec]
  extends AbstractRedisCommand(bulk[A])

abstract class RedisOptDataCommand[A: RedisDataCodec]
  extends AbstractRedisCommand(nullBulkOr[A])

abstract class RedisDataSeqCommand[A: RedisDataCodec]
  extends AbstractRedisCommand[Seq[A]](multiBulk[A])

abstract class RedisOptDataSeqCommand[A: RedisDataCodec]
  extends AbstractRedisCommand[Seq[Opt[A]]](multiBulk(nullBulkOr[A]))

abstract class RedisRawCommand
  extends AbstractRedisCommand[ValidRedisMsg](undecoded)

abstract class RedisNothingCommand
  extends AbstractRedisCommand(failing)

abstract class RedisUnitCommand
  extends AbstractRedisCommand[Unit](simpleOkUnit)

abstract class RedisLongCommand
  extends AbstractRedisCommand[Long](integerLong)

abstract class RedisIntCommand
  extends AbstractRedisCommand[Int](integerInt)

abstract class RedisOptLongCommand
  extends AbstractRedisCommand[Opt[Long]](nullBulkOr(integerLong))

abstract class RedisOptDoubleCommand
  extends AbstractRedisCommand[Opt[Double]](nullOrEmptyBulkOr(bulkDouble))

abstract class RedisOptStringCommand
  extends AbstractRedisCommand[Opt[String]](nullBulkOr(bulkUTF8))

abstract class RedisBooleanCommand
  extends AbstractRedisCommand[Boolean](integerBoolean)

abstract class RedisSimpleStringCommand
  extends AbstractRedisCommand[String](simpleUTF8)

abstract class RedisBinaryCommand
  extends AbstractRedisCommand[ByteString](bulkBinary)

abstract class RedisDoubleCommand
  extends AbstractRedisCommand[Double](bulkDouble)

abstract class RedisScanCommand[T](decoder: ReplyDecoder[T])
  extends AbstractRedisCommand[(Cursor, T)](multiBulkPair(bulkCursor, decoder))

abstract class RedisSeqCommand[T](elementDecoder: ReplyDecoder[T])
  extends AbstractRedisCommand[Seq[T]](multiBulk(elementDecoder))

abstract class RedisOptSeqCommand[T](elementDecoder: ReplyDecoder[T])
  extends AbstractRedisCommand[Opt[Seq[T]]](nullMultiBulkOr(multiBulk(elementDecoder)))

abstract class RedisOptCommand[T](elementDecoder: ReplyDecoder[T])
  extends AbstractRedisCommand[Opt[T]](nullBulkOr(elementDecoder))

abstract class RedisEnumCommand[E <: NamedEnum](companion: NamedEnumCompanion[E])
  extends AbstractRedisCommand[E](bulkNamedEnum(companion))
