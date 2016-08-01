package com.avsystem.commons
package redis

import akka.util.ByteString
import com.avsystem.commons.misc.{NamedEnum, Opt}
import com.avsystem.commons.redis.CommandEncoder.CommandArg
import com.avsystem.commons.redis.RedisBatch.Index
import com.avsystem.commons.redis.exception.{ErrorReplyException, UnexpectedReplyException}
import com.avsystem.commons.redis.protocol._

import scala.collection.mutable.ArrayBuffer

trait RedisCommand[+A, -S] extends AtomicBatch[A, S] with RawCommand {
  def decodeExpected: PartialFunction[ValidRedisMsg, A]

  def decode(replyMsg: RedisReply): A = replyMsg match {
    case validReply: ValidRedisMsg =>
      decodeExpected.applyOrElse(validReply, (r: ValidRedisMsg) => throw new UnexpectedReplyException(r.toString))
    case err: ErrorMsg =>
      throw new ErrorReplyException(err)
    case error: FailureReply =>
      throw error.exception
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

  def key(key: ByteString) = fluent(buffer += CommandKeyMsg(key))
  def keys(keys: TraversableOnce[ByteString]) = fluent(keys.foreach(key))
  def add[T: CommandArg](value: T): CommandEncoder = fluent(CommandArg.add(this, value))
  def addFlag(flag: String, value: Boolean): CommandEncoder = if (value) add(flag) else this
  def optAdd[T: CommandArg](flag: String, value: Opt[T]) = fluent(value.foreach(t => add(flag).add(t)))
  def optKey(flag: String, value: Opt[ByteString]) = fluent(value.foreach(t => add(flag).key(t)))
  def keyValues[T: CommandArg](keyValues: TraversableOnce[(ByteString, T)]) = fluent(keyValues.foreach({ case (k, v) => key(k).add(v) }))
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

trait RedisRawCommand[S] extends RedisCommand[ValidRedisMsg, S] {
  def decodeExpected = {
    case msg => msg
  }
}

trait RedisUnitCommand[S] extends RedisCommand[Unit, S] {
  def decodeExpected = {
    case RedisMsg.Ok => ()
  }
}

trait RedisLongCommand[S] extends RedisCommand[Long, S] {
  def decodeExpected = {
    case IntegerMsg(res) => res
  }
}

trait RedisOptLongCommand[S] extends RedisCommand[Opt[Long], S] {
  def decodeExpected = {
    case IntegerMsg(res) => Opt(res)
    case NullBulkStringMsg => Opt.Empty
  }
}

trait RedisBooleanCommand[S] extends RedisCommand[Boolean, S] {
  def decodeExpected = {
    case IntegerMsg(0) => false
    case IntegerMsg(1) => true
  }
}

trait RedisSimpleStringCommand[S] extends RedisCommand[ByteString, S] {
  def decodeExpected = {
    case SimpleStringMsg(data) => data
  }
}

trait RedisBinaryCommand[S] extends RedisCommand[ByteString, S] {
  def decodeExpected = {
    case BulkStringMsg(data) => data
  }
}

trait RedisOptBinaryCommand[S] extends RedisCommand[Opt[ByteString], S] {
  def decodeExpected = {
    case BulkStringMsg(data) => Opt(data)
    case NullBulkStringMsg => Opt.Empty
  }
}

trait RedisBinarySeqCommand[S] extends RedisCommand[Seq[ByteString], S] {
  def decodeExpected = {
    case ArrayMsg(elements) =>
      elements.map {
        case BulkStringMsg(bs) => bs
        case msg => throw new UnexpectedReplyException(s"Expected multi bulk reply, but one of the elements is $msg")
      }
  }
}

trait RedisOptBinarySeqCommand[S] extends RedisCommand[Seq[Opt[ByteString]], S] {
  def decodeExpected = {
    case ArrayMsg(elements) =>
      elements.map {
        case BulkStringMsg(bs) => Opt(bs)
        case NullBulkStringMsg => Opt.Empty
        case msg => throw new UnexpectedReplyException(s"Expected multi bulk reply, but one of the elements is $msg")
      }
  }
}

trait RedisDoubleCommand[S] extends RedisCommand[Double, S] {
  def decodeExpected = {
    case BulkStringMsg(bytes) => bytes.utf8String.toDouble
  }
}
