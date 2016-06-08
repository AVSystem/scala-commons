package com.avsystem.commons
package redis

import akka.util.{ByteString, Timeout}
import com.avsystem.commons.misc.Sam
import com.avsystem.commons.redis.RedisBatch.{MessageBuffer, RepliesDecoder, Transaction}
import com.avsystem.commons.redis.commands.{Exec, Multi}
import com.avsystem.commons.redis.exception.{OptimisticLockException, UnexpectedReplyException}
import com.avsystem.commons.redis.protocol._

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future

sealed trait Scope
object Scope {
  sealed trait Connection extends Scope
  sealed trait Node extends Connection
  sealed trait Cluster extends Node
}

/**
  * Represents a Redis operation or a set of operations which is sent to Redis as a single batch.
  */
trait RedisBatch[+A, +S] {self =>
  /**
    * Encodes itself to a sequence of redis messages that will be sent in one batch to the server.
    * Returns a [[RepliesDecoder]] responsible for translating raw Redis responses into the actual result.
    */
  def encodeCommands(messageBuffer: MessageBuffer, inTransaction: Boolean): RepliesDecoder[A]

  def map2[B, S0 >: S, C](other: RedisBatch[B, S0])(f: (A, B) => C): RedisBatch[C, S0] =
    new RedisBatch[C, S0] {
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

  def <*[B, S0 >: S](other: RedisBatch[B, S0]): RedisBatch[A, S0] =
    map2(other)((a, _) => a)

  def *>[B, S0 >: S](other: RedisBatch[B, S0]): RedisBatch[B, S0] =
    map2(other)((_, b) => b)

  def map[B](f: A => B): RedisBatch[B, S] =
    new RedisBatch[B, S] {
      def encodeCommands(messageBuffer: MessageBuffer, inTransaction: Boolean) = {
        val decoder = self.encodeCommands(messageBuffer, inTransaction)
        RepliesDecoder((replies, start, end) => f(decoder.decodeReplies(replies, start, end)))
      }
    }

  /**
    * Returns a batch which invokes the same Redis commands as this batch, but ensures that
    * they are invoked inside a Redis transaction (`MULTI`-`EXEC`).
    */
  def transaction: RedisBatch[A, S] =
    new Transaction(this)

  def operation: RedisOp[A, S] =
    RedisOp.LeafOp(this)
}

object RedisBatch {
  final class Transaction[+A, +S](batch: RedisBatch[A, S]) extends RedisBatch[A, S] {
    def encodeCommands(messageBuffer: MessageBuffer, inTransaction: Boolean) =
      if (inTransaction) {
        batch.encodeCommands(messageBuffer, inTransaction)
      } else {
        messageBuffer += ArrayMsg(Multi.encode)
        val initialSize = messageBuffer.size
        val actualDecoder = batch.encodeCommands(messageBuffer, inTransaction = true)
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

  def success[A](a: A): RedisBatch[A, Nothing] =
    new RedisBatch[A, Nothing] with RedisBatch.RepliesDecoder[A] {
      def encodeCommands(messageBuffer: MessageBuffer, inTransaction: Boolean) = this
      def decodeReplies(replies: IndexedSeq[RedisMsg], start: Int, end: Int) = a
    }

  def failure(cause: Throwable): RedisBatch[Nothing, Nothing] =
    new RedisBatch[Nothing, Nothing] with RedisBatch.RepliesDecoder[Nothing] {
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
sealed trait RedisOp[+A, +S] {
  def flatMap[B, S0 >: S](f: A => RedisOp[B, S0]): RedisOp[B, S0]

  def map[B](f: A => B): RedisOp[B, S] =
    flatMap(a => RedisOp.success(f(a)))

  //TODO more API
}

object RedisOp {
  implicit class executeNodeOp[A](private val op: RedisOp[A, Scope.Node]) extends AnyVal {
    def executeWith(client: RedisNodeClient)(implicit timeout: Timeout): Future[A] =
      client.execute(op)
  }

  def success[A](a: A): RedisOp[A, Nothing] =
    RedisBatch.success(a).operation

  def failure(cause: Throwable): RedisOp[Nothing, Nothing] =
    RedisBatch.failure(cause).operation

  //TODO more API

  case class Watch(keys: ByteString*) {
    def flatMap[A, S](f: Unit => RedisOp[A, S]): RedisOp[A, S] = f(()) match {
      case leaf: LeafOp[A, S] => leaf.copy(batch = leaf.batch)
      case fm: FlatMappedOp[_, A, S] => fm.copy(batch = fm.batch)
    }
  }
  case class LeafOp[A, S](batch: RedisBatch[A, S]) extends RedisOp[A, S] {
    def flatMap[B, S0 >: S](f: A => RedisOp[B, S0]) = FlatMappedOp(batch, f)
  }
  case class FlatMappedOp[A, B, S](batch: RedisBatch[A, S], fun: A => RedisOp[B, S]) extends RedisOp[B, S] {
    def flatMap[C, S0 >: S](f: B => RedisOp[C, S0]) = FlatMappedOp(batch, (a: A) => fun(a).flatMap(f))
  }
}
