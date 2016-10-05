package com.avsystem.commons
package redis

import akka.util.ByteString
import com.avsystem.commons.misc.{NamedEnum, Opt}
import com.avsystem.commons.redis.CommandEncoder.CommandArg
import com.avsystem.commons.redis.RedisBatch.Index
import com.avsystem.commons.redis.exception.{ErrorReplyException, UnexpectedReplyException}
import com.avsystem.commons.redis.protocol._

import scala.collection.mutable.ArrayBuffer

trait RedisCommand[+A] extends AtomicBatch[A] with RawCommand {
  protected def decodeExpected: PartialFunction[ValidRedisMsg, A]

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

trait HasCodec[A] {
  protected def codec: RedisDataCodec[A]
}

trait RedisDataCommand[A] extends RedisCommand[A] with HasCodec[A] {
  protected def decodeExpected = {
    case BulkStringMsg(data) => codec.read(data)
  }
}

trait RedisOptDataCommand[A] extends RedisOptCommand[A] with HasCodec[A] {
  protected def decodeNonEmpty(bytes: ByteString) = codec.read(bytes)
}

trait RedisDataSeqCommand[A] extends RedisSeqCommand[A] with HasCodec[A] {
  protected val decodeElement: PartialFunction[ValidRedisMsg, A] = {
    case BulkStringMsg(bs) => codec.read(bs)
  }
}

trait RedisOptDataSeqCommand[A] extends RedisCommand[Seq[Opt[A]]] with HasCodec[A] {
  protected def decodeExpected = {
    case ArrayMsg(elements) =>
      elements.map {
        case BulkStringMsg(bs) => Opt(codec.read(bs))
        case NullBulkStringMsg => Opt.Empty
        case msg => throw new UnexpectedReplyException(s"Expected multi bulk reply, but one of the elements is $msg")
      }
  }
}

trait RedisRawCommand extends RedisCommand[ValidRedisMsg] {
  def decodeExpected = {
    case msg => msg
  }
}

trait RedisUnitCommand extends RedisCommand[Unit] {
  def decodeExpected = {
    case RedisMsg.Ok => ()
  }
}

trait RedisLongCommand extends RedisCommand[Long] {
  def decodeExpected = {
    case IntegerMsg(res) => res
  }
}

trait RedisOptLongCommand extends RedisCommand[Opt[Long]] {
  def decodeExpected = {
    case IntegerMsg(res) => Opt(res)
    case NullBulkStringMsg => Opt.Empty
  }
}

trait RedisOptStringCommand extends RedisOptCommand[String] {
  protected def decodeNonEmpty(bytes: ByteString) = bytes.utf8String
}

trait RedisBooleanCommand extends RedisCommand[Boolean] {
  def decodeExpected = {
    case IntegerMsg(0) => false
    case IntegerMsg(1) => true
  }
}

trait RedisSimpleStringCommand extends RedisCommand[String] {
  def decodeExpected = {
    case SimpleStringMsg(data) => data.utf8String
  }
}

trait RedisBinaryCommand extends RedisCommand[ByteString] {
  def decodeExpected = {
    case BulkStringMsg(data) => data
  }
}

trait RedisOptCommand[A] extends RedisCommand[Opt[A]] {
  def decodeExpected = {
    case BulkStringMsg(data) => Opt(decodeNonEmpty(data))
    case NullBulkStringMsg => Opt.Empty
  }
  protected def decodeNonEmpty(bytes: ByteString): A
}

trait RedisSeqCommand[A] extends RedisCommand[Seq[A]] {
  def decodeExpected = {
    case ArrayMsg(elements) => elements.map {
      case validReply: ValidRedisMsg =>
        decodeElement.applyOrElse(validReply, (r: ValidRedisMsg) => throw new UnexpectedReplyException(r.toString))
      case errMsg: ErrorMsg =>
        throw new ErrorReplyException(errMsg)
    }
  }

  protected def decodeElement: PartialFunction[ValidRedisMsg, A]
}

trait RedisDoubleCommand extends RedisCommand[Double] {
  def decodeExpected = {
    case BulkStringMsg(bytes) => bytes.utf8String.toDouble
  }
}

trait RedisOptDoubleCommand extends RedisCommand[Opt[Double]] {
  def decodeExpected = {
    case BulkStringMsg(ByteString.empty) => Opt.Empty
    case BulkStringMsg(bytes) => bytes.utf8String.toDouble.opt
    case NullBulkStringMsg => Opt.Empty
  }
}
