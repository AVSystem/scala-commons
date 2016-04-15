package com.avsystem.commons
package redis

import akka.util.{ByteString, Timeout}
import com.avsystem.commons.misc.{NamedEnum, Opt, Sam}
import com.avsystem.commons.redis.CommandEncoder.CommandArg
import com.avsystem.commons.redis.RedisFlushable.{MessageBuffer, RepliesDecoder, Transaction}
import com.avsystem.commons.redis.commands.{Exec, Multi}
import com.avsystem.commons.redis.exception.{ErrorReplyException, OptimisticLockException, UnexpectedReplyException}
import com.avsystem.commons.redis.protocol._

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future

/**
  * Represents a Redis operation or a set of operations which is sent to Redis as a single batch.
  */
trait RedisFlushable[+A] {self =>
  /**
    * Encodes itself to a sequence of redis messages that will be sent in one batch to the server.
    * Returns a [[RepliesDecoder]] responsible for translating raw Redis responses into the actual result.
    */
  def encodeCommands(messageBuffer: MessageBuffer, inTransaction: Boolean): RepliesDecoder[A]

  def map2[B, C](other: RedisFlushable[B])(f: (A, B) => C): RedisFlushable[C] =
    new RedisFlushable[C] {
      def encodeCommands(messageBuffer: MessageBuffer, inTransaction: Boolean) = {
        val initialSize = messageBuffer.size
        val selfDecoder = self.encodeCommands(messageBuffer, inTransaction)
        val selfCount = messageBuffer.size - initialSize
        val otherDecoder = other.encodeCommands(messageBuffer, inTransaction)
        RepliesDecoder { (replies, start, end) =>
          val a = selfDecoder.decodeReplies(replies, start, start + selfCount)
          val b = otherDecoder.decodeReplies(replies, start + selfCount, end)
          f(a, b)
        }
      }
    }

  def <*[B](other: RedisFlushable[B]): RedisFlushable[A] =
    map2(other)((a, _) => a)

  def *>[B](other: RedisFlushable[B]): RedisFlushable[B] =
    map2(other)((_, b) => b)

  def map[B](f: A => B): RedisFlushable[B] =
    new RedisFlushable[B] {
      def encodeCommands(messageBuffer: MessageBuffer, inTransaction: Boolean) = {
        val decoder = self.encodeCommands(messageBuffer, inTransaction)
        RepliesDecoder((replies, start, end) => f(decoder.decodeReplies(replies, start, end)))
      }
    }

  /**
    * Returns a flushable which invokes the same Redis commands as this flushable, but ensures that
    * they are invoked inside a Redis transaction (`MULTI`-`EXEC`).
    */
  def transaction: RedisFlushable[A] =
    new Transaction(this)

  def operation: RedisOp[A] =
    RedisOp.LeafOp(this)
}

object RedisFlushable {
  final class Transaction[+A](flushable: RedisFlushable[A]) extends RedisFlushable[A] {
    def encodeCommands(messageBuffer: MessageBuffer, inTransaction: Boolean) =
      if (inTransaction) {
        flushable.encodeCommands(messageBuffer, inTransaction)
      } else {
        messageBuffer += ArrayMsg(Multi.encode)
        val initialSize = messageBuffer.size
        val actualDecoder = flushable.encodeCommands(messageBuffer, inTransaction = true)
        val actualSize = messageBuffer.size - initialSize
        messageBuffer += ArrayMsg(Exec.encode)
        RepliesDecoder((replies, start, end) => replies(end - 1) match {
          case ArrayMsg(actualReplies) => actualDecoder.decodeReplies(actualReplies, 0, actualSize)
          case NullArrayMsg => throw new OptimisticLockException
          case errorMsg: ErrorMsg =>
            val actualErrors = replies.iterator.drop(start + 1).take(actualSize).map {
              case SimpleStringMsg(str) if str.utf8String == "QUEUED" => errorMsg
              case actualErrorMsg: ErrorMsg => actualErrorMsg
              case msg => throw new UnexpectedReplyException(s"expected simple string QUEUED or error, got $msg")
            }.to[ArrayBuffer]
            actualDecoder.decodeReplies(actualErrors, 0, actualSize)
          case _ => throw new UnexpectedReplyException("expected multi bulk reply")
        })
      }

    override def transaction = this
  }

  class MessageBuffer(private val buffer: ArrayBuffer[RedisMsg]) extends AnyVal {
    def result: IndexedSeq[RedisMsg] = buffer
    def +=(msg: RedisMsg): Unit = buffer += msg
    def ++=(msgs: TraversableOnce[RedisMsg]): Unit = buffer ++= msgs
    def size = buffer.size
  }

  trait RepliesDecoder[+A] {
    def decodeReplies(replies: IndexedSeq[RedisMsg], start: Int, end: Int): A
  }
  object RepliesDecoder {
    def apply[A](f: (IndexedSeq[RedisMsg], Int, Int) => A): RepliesDecoder[A] = Sam[RepliesDecoder[A]](f)
  }

  def success[A](a: A): RedisFlushable[A] =
    new RedisFlushable[A] with RedisFlushable.RepliesDecoder[A] {
      def encodeCommands(messageBuffer: MessageBuffer, inTransaction: Boolean) = this
      def decodeReplies(replies: IndexedSeq[RedisMsg], start: Int, end: Int) = a
    }

  def failure(cause: Throwable): RedisFlushable[Nothing] =
    new RedisFlushable[Nothing] with RedisFlushable.RepliesDecoder[Nothing] {
      def encodeCommands(messageBuffer: MessageBuffer, inTransaction: Boolean) = this
      def decodeReplies(replies: IndexedSeq[RedisMsg], start: Int, end: Int) = throw cause
    }

  //TODO more API
  //TODO sequence
}

/**
  * Represents a sequence of Redis operations executed using a single Redis connection.
  * Any operation may depend on the result of some previous operation (therefore, `flatMap` is available).
  */
sealed trait RedisOp[+A] {
  def flatMap[B](f: A => RedisOp[B]): RedisOp[B]

  def map[B](f: A => B): RedisOp[B] =
    flatMap(a => RedisOp.success(f(a)))

  def executeWith(client: RedisNodeClient)(implicit timeout: Timeout): Future[A] =
    client.execute(this)

  //TODO more API
}

object RedisOp {
  def success[A](a: A): RedisOp[A] =
    RedisFlushable.success(a).operation

  def failure(cause: Throwable): RedisOp[Nothing] =
    RedisFlushable.failure(cause).operation

  //TODO more API

  case class LeafOp[A](flushable: RedisFlushable[A]) extends RedisOp[A] {
    def flatMap[B](f: A => RedisOp[B]) = FlatMappedOp(flushable, f)
  }
  case class FlatMappedOp[A, B](flushable: RedisFlushable[A], fun: A => RedisOp[B]) extends RedisOp[B] {
    def flatMap[C](f: B => RedisOp[C]) = FlatMappedOp(flushable, (a: A) => fun(a).flatMap(f))
  }
}

trait RedisCommand[+A] extends RedisFlushable[A] with RedisFlushable.RepliesDecoder[A] {
  def encode: IndexedSeq[BulkStringMsg]
  def isKey(idx: Int): Boolean
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

  def decodeReplies(replies: IndexedSeq[RedisMsg], start: Int, end: Int) =
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

trait RedisRawCommand extends RedisCommand[ValidRedisMsg] {
  def decodeExpected = {
    case msg => msg
  }
}

trait RedisUnitCommand extends RedisCommand[Unit] {
  def decodeExpected = {
    case SimpleStringStr("OK") => ()
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

trait RedisBooleanCommand extends RedisCommand[Boolean] {
  def decodeExpected = {
    case IntegerMsg(0) => false
    case IntegerMsg(1) => true
  }
}

trait RedisBinaryCommand extends RedisCommand[ByteString] {
  def decodeExpected = {
    case BulkStringMsg(data) => data
  }
}

trait RedisOptBinaryCommand extends RedisCommand[Opt[ByteString]] {
  def decodeExpected = {
    case BulkStringMsg(data) => Opt(data)
    case NullBulkStringMsg => Opt.Empty
  }
}

trait RedisBinarySeqCommand extends RedisCommand[Seq[ByteString]] {
  def decodeExpected = {
    case ArrayMsg(elements) =>
      elements.map {
        case BulkStringMsg(bs) => bs
        case msg => throw new UnexpectedReplyException(s"Expected multi bulk reply, but one of the elements is $msg")
      }
  }
}

trait RedisOptBinarySeqCommand extends RedisCommand[Seq[Opt[ByteString]]] {
  def decodeExpected = {
    case ArrayMsg(elements) =>
      elements.map {
        case BulkStringMsg(bs) => Opt(bs)
        case NullBulkStringMsg => Opt.Empty
        case msg => throw new UnexpectedReplyException(s"Expected multi bulk reply, but one of the elements is $msg")
      }
  }
}

trait RedisDoubleCommand extends RedisCommand[Double] {
  def decodeExpected = {
    case BulkStringMsg(bytes) => bytes.utf8String.toDouble
  }
}

trait SimpleSingleKeyed {this: RedisCommand[_] =>
  def isKey(idx: Int) = idx == 1
}
