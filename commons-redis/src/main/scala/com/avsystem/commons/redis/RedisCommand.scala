package com.avsystem.commons
package redis

import akka.util.ByteString
import com.avsystem.commons.misc.{NamedEnum, Opt}
import com.avsystem.commons.redis.CommandEncoder.CommandArg
import com.avsystem.commons.redis.RedisBatch.{ConnectionState, MessageBuffer}
import com.avsystem.commons.redis.exception.{ErrorReplyException, UnexpectedReplyException}
import com.avsystem.commons.redis.protocol._

import scala.collection.mutable.ArrayBuffer

trait RedisCommand[+A, -S] extends AtomicBatch[A, S] with RedisBatch.RepliesDecoder[A] {
  def encode: IndexedSeq[BulkStringMsg]
  def decodeExpected: PartialFunction[ValidRedisMsg, A]

  protected def encoder(commandName: String*): CommandEncoder = {
    val res = new CommandEncoder(new ArrayBuffer)
    res.add(commandName)
    res
  }

  def decode(replyMsg: RedisMsg): A = replyMsg match {
    case validReply: ValidRedisMsg =>
      decodeExpected.applyOrElse(validReply, (r: ValidRedisMsg) => throw new UnexpectedReplyException(r.toString))
    case err: ErrorMsg =>
      throw new ErrorReplyException(err)
  }

  def encodeCommands(messageBuffer: MessageBuffer, inTransaction: Boolean) = {
    messageBuffer += ArrayMsg(encode)
    this
  }

  def decodeReplies(replies: IndexedSeq[RedisMsg], start: Int, end: Int, state: ConnectionState) =
    decode(replies(start))

  override def toString =
    encode.iterator.map(bs => RedisMsg.escape(bs.string)).mkString(" ")
}

class CommandEncoder(val result: ArrayBuffer[BulkStringMsg]) extends AnyVal {
  private def fluent(code: => Any): CommandEncoder = {
    code
    this
  }
  def add[T: CommandArg](value: T): CommandEncoder = fluent(CommandArg.add(this, value))
  def addFlag(flag: String, value: Boolean): CommandEncoder = if (value) add(flag) else this
  def optAdd[T: CommandArg](flag: String, value: Opt[T]) = fluent(value.foreach(t => add(flag).add(t)))
}

object CommandEncoder {
  case class CommandArg[-T](add: (CommandEncoder, T) => Any) extends AnyVal
  object CommandArg {
    def add[T](ce: CommandEncoder, value: T)(implicit ca: CommandArg[T]): Unit =
      ca.add(ce, value)

    implicit val ByteStringArg: CommandArg[ByteString] = CommandArg((ce, v) => ce.result += BulkStringMsg(v))
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
    case SimpleStringStr("OK") => ()
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

trait Unkeyed {this: RedisCommand[Any, Nothing] =>
  def reportKeys(consumer: ByteString => Any) = ()
}

trait SimpleSingleKeyed {this: RedisCommand[Any, Nothing] =>
  def key: ByteString
  def reportKeys(consumer: ByteString => Any): Unit = consumer(key)
}

trait SimpleMultiKeyed {this: RedisCommand[Any, Nothing] =>
  def keys: Seq[ByteString]
  def reportKeys(consumer: ByteString => Any): Unit = keys.foreach(consumer)
}
