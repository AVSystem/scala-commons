package com.avsystem.commons
package redis

import akka.util.ByteString
import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion}
import com.avsystem.commons.redis.CommandEncoder.CommandArg
import com.avsystem.commons.redis.RedisBatch.Index
import com.avsystem.commons.redis.commands.{Cursor, ReplyDecoder}
import com.avsystem.commons.redis.exception.{ErrorReplyException, UnexpectedReplyException}
import com.avsystem.commons.redis.protocol._

import scala.collection.compat._
import scala.collection.mutable

trait RedisCommand[+A] extends SinglePackBatch[A] with RawCommand { self =>
  protected def decodeExpected: ReplyDecoder[A]

  final protected def decode(replyMsg: RedisReply): A = replyMsg match {
    case validReply: ValidRedisMsg =>
      decodeExpected.applyOrElse(validReply, (r: ValidRedisMsg) =>
        throw new UnexpectedReplyException(r.toString))
    case err: ErrorMsg =>
      throw new ErrorReplyException(err, this)
    case error: FailureReply =>
      throw error.exception
    case _ =>
      throw new UnexpectedReplyException(replyMsg.toString)
  }

  final def decodeReplies(replies: Int => RedisReply, idx: Index, inTransaction: Boolean): A =
    decode(replies(idx.inc()))

  override def toString: String =
    encoded.elements.iterator.map(bs => RedisMsg.escape(bs.string)).mkString(" ")

  override def map[B](fun: A => B): RedisCommand[B] =
    new RedisCommand[B] {
      def level: RawCommand.Level = self.level
      def encoded: ArrayMsg[BulkStringMsg] = self.encoded
      def decodeExpected: ReplyDecoder[B] = self.decodeExpected andThen fun
      override def immediateResult: Opt[B] =
        self.immediateResult.map(fun)
      override def updateWatchState(message: RedisMsg, state: WatchState): Unit =
        self.updateWatchState(message, state)
      override def maxBlockingMillis: Int =
        self.maxBlockingMillis
    }

  /**
    * Returns optional value that may be used as immediate result of this command if it command can be treated as
    * no-op. For example `DEL` command with no keys may simply return 0 as its immediate result.
    */
  def immediateResult: Opt[A] = Opt.Empty

  protected[this] final def whenEmpty(args: IterableOnce[Any], value: A): Opt[A] =
    if (args.iterator.isEmpty) Opt(value) else Opt.Empty

  final def batchOrFallback: RedisBatch[A] =
    immediateResult.fold(this: RedisBatch[A])(RedisBatch.success)
}

abstract class AbstractRedisCommand[A](protected val decodeExpected: ReplyDecoder[A])
  extends RedisCommand[A]

final class CommandEncoder(private val builder: mutable.ArrayBuilder[BulkStringMsg]) extends AnyVal {
  def result: ArrayMsg[BulkStringMsg] = ArrayMsg(IArraySeq.unsafeWrapArray(builder.result()))

  private def fluent(code: => Unit): CommandEncoder = {
    code
    this
  }

  def key[K: RedisDataCodec](k: K): CommandEncoder =
    fluent(builder += CommandKeyMsg(RedisDataCodec.write(k)))
  def keys[K: RedisDataCodec](keys: IterableOnce[K]): CommandEncoder =
    fluent(keys.iterator.foreach(key[K]))
  def data[V: RedisDataCodec](v: V): CommandEncoder =
    fluent(builder += BulkStringMsg(RedisDataCodec.write(v)))
  def datas[V: RedisDataCodec](values: IterableOnce[V]): CommandEncoder =
    fluent(values.iterator.foreach(data[V]))
  def add[T: CommandArg](value: T): CommandEncoder =
    fluent(CommandArg.add(this, value))
  def addFlag(flag: String, value: Boolean): CommandEncoder =
    if (value) add(flag) else this
  def optAdd[T: CommandArg](value: Opt[T]): CommandEncoder =
    fluent(value.foreach(t => add(t)))
  def optAdd[T: CommandArg](flag: String, value: Opt[T]): CommandEncoder =
    fluent(value.foreach(t => add(flag).add(t)))
  def optAdd[T: CommandArg, D: CommandArg](value: Opt[T], default: D): CommandEncoder =
    fluent(value.fold(CommandArg.add(this, default))(CommandArg.add(this, _)))
  def optKey[K: RedisDataCodec](flag: String, value: Opt[K]): CommandEncoder =
    fluent(value.foreach(t => add(flag).key(t)))
  def optData[V: RedisDataCodec](flag: String, value: Opt[V]): CommandEncoder =
    fluent(value.foreach(t => add(flag).data(t)))
  def keyDatas[K: RedisDataCodec, V: RedisDataCodec](keyDatas: IterableOnce[(K, V)]): CommandEncoder =
    fluent(keyDatas.iterator.foreach({ case (k, v) => key(k).data(v) }))
  def dataPairs[K: RedisDataCodec, V: RedisDataCodec](dataPairs: IterableOnce[(K, V)]): CommandEncoder =
    fluent(dataPairs.iterator.foreach({ case (k, v) => data(k).data(v) }))
  def dataPairs[R: RedisRecordCodec](record: R): CommandEncoder =
    fluent(builder ++= RedisRecordCodec[R].write(record))
  def argDataPairs[T: CommandArg, V: RedisDataCodec](argDataPairs: IterableOnce[(T, V)]): CommandEncoder =
    fluent(argDataPairs.iterator.foreach({ case (a, v) => add(a).data(v) }))
}

object CommandEncoder {
  case class CommandArg[-T](add: (CommandEncoder, T) => Any) extends AnyVal
  object CommandArg {
    def add[T](ce: CommandEncoder, value: T)(implicit ca: CommandArg[T]): Unit =
      ca.add(ce, value)

    implicit val ByteStringArg: CommandArg[ByteString] = CommandArg((ce, v) => ce.builder += BulkStringMsg(v))
    implicit val BooleanArg: CommandArg[Boolean] = CommandArg((ce, v) => ce.add(if (v) 1 else 0))
    implicit val StringArg: CommandArg[String] = CommandArg((ce, v) => ce.add(ByteString(v)))
    implicit val IntArg: CommandArg[Int] = CommandArg((ce, v) => ce.add(v.toString))
    implicit val LongArg: CommandArg[Long] = CommandArg((ce, v) => ce.add(v.toString))
    implicit val DoubleArg: CommandArg[Double] = CommandArg((ce, v) => ce.add(v.toString))
    implicit val NamedEnumArg: CommandArg[NamedEnum] = CommandArg((ce, v) => ce.add(v.name))
    implicit def CollArg[T: CommandArg]: CommandArg[IterableOnce[T]] =
      CommandArg((ce, v) => v.iterator.foreach(t => ce.add(t)))

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
  extends AbstractRedisCommand[Seq[A]](multiBulkAsSeq[A])

abstract class RedisDataSetCommand[A: RedisDataCodec]
  extends AbstractRedisCommand[BSet[A]](multiBulkAsSet[A])

abstract class RedisOptDataSeqCommand[A: RedisDataCodec]
  extends AbstractRedisCommand[Seq[Opt[A]]](multiBulkAsSeq(nullBulkOr[A]))

abstract class RedisRawCommand
  extends AbstractRedisCommand[ValidRedisMsg](undecoded)

abstract class RedisNothingCommand
  extends AbstractRedisCommand(failing)

abstract class RedisUnitCommand
  extends AbstractRedisCommand[Unit](simpleOkAsUnit)

abstract class RedisLongCommand
  extends AbstractRedisCommand[Long](integerAsLong)

abstract class RedisIntCommand
  extends AbstractRedisCommand[Int](integerAsInt)

abstract class RedisOptLongCommand
  extends AbstractRedisCommand[Opt[Long]](nullBulkOr(integerAsLong))

abstract class RedisPositiveLongCommand
  extends AbstractRedisCommand[Opt[Long]](positiveIntegerAsLongOpt)

abstract class RedisOptDoubleCommand
  extends AbstractRedisCommand[Opt[Double]](nullOrEmptyBulkOr(bulkAsDouble))

abstract class RedisOptStringCommand
  extends AbstractRedisCommand[Opt[String]](nullBulkOr(bulkAsUTF8))

abstract class RedisBooleanCommand
  extends AbstractRedisCommand[Boolean](integerAsBoolean)

abstract class RedisSimpleStringCommand
  extends AbstractRedisCommand[String](simpleAsUTF8)

abstract class RedisBinaryCommand
  extends AbstractRedisCommand[ByteString](bulkAsBinary)

abstract class RedisDoubleCommand
  extends AbstractRedisCommand[Double](bulkAsDouble)

abstract class RedisScanCommand[T](decoder: ReplyDecoder[Seq[T]])
  extends AbstractRedisCommand[(Cursor, Seq[T])](multiBulkAsPair(bulkAsCursor, decoder))

abstract class RedisSeqCommand[T](elementDecoder: ReplyDecoder[T])
  extends AbstractRedisCommand[Seq[T]](multiBulkAsSeq(elementDecoder))

abstract class RedisOptSeqCommand[T](elementDecoder: ReplyDecoder[T])
  extends AbstractRedisCommand[Opt[Seq[T]]](nullMultiBulkOr(multiBulkAsSeq(elementDecoder)))

abstract class RedisOptCommand[T](elementDecoder: ReplyDecoder[T])
  extends AbstractRedisCommand[Opt[T]](nullBulkOr(elementDecoder))

abstract class RedisEnumCommand[E <: NamedEnum](companion: NamedEnumCompanion[E])
  extends AbstractRedisCommand[E](bulkAsNamedEnum(companion))
