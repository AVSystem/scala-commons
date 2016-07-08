package com.avsystem.commons
package redis

import akka.util.ByteString
import com.avsystem.commons.misc.Sam
import com.avsystem.commons.redis.RedisBatch.{MessageBuffer, RepliesDecoder, Transaction}
import com.avsystem.commons.redis.commands.{Exec, Multi}
import com.avsystem.commons.redis.exception.{OptimisticLockException, UnexpectedReplyException}
import com.avsystem.commons.redis.protocol._

import scala.collection.mutable.ArrayBuffer

/**
  * A [[Scope]] is associated with every [[RedisCommand]] and propagated to [[RedisBatch]] and [[RedisOp]].
  * Scope determines which type of client can execute that command/batch/operation.
  * <p/>
  * Commands scoped as [[Scope.Connection]] change state of Redis connection and therefore can be executed only
  * on single-connection clients. They can't be executed by a node client (which uses connection pool) or by a
  * cluster client. Examples of such commands include `AUTH`, `CLIENT SETNAME`, `SELECT`.
  * <p/>
  * Commands scoped as [[Scope.Node]] can be executed by a connection client and also by a node client (connection pool)
  * but cannot be executed by a cluster client, because they are not associated with any Redis key.
  * These are mostly configuration and maintenance commands like `CLIENT LIST`, `INFO`, `FLUSHDB`, `CLUSTER NODES`, etc.
  * <p/>
  * Commands scoped as [[Scope.Cluster]] can be executed by any type of client. In particular they can be executed by
  * a cluster client, because they contain at least one Redis key which can be used to determine appropriate cluster
  * node for handling this command. Most of the commands that operate on actual data held in Redis are scoped as
  * [[Scope.Cluster]].
  */
sealed trait Scope
object Scope {
  sealed trait Empty extends Scope
  sealed trait Cluster extends Empty
  sealed trait Node extends Cluster
  sealed trait Operation extends Node
  sealed trait Connection extends Operation
}

/**
  * Represents a Redis operation or a set of operations which is sent to Redis as a single batch.
  *
  * @tparam A result yield by this batch
  * @tparam S [[Scope]] of this batch ([[Scope.Connection]], [[Scope.Node]] or [[Scope.Cluster]])
  */
trait RedisBatch[+A, -S] {self =>
  /**
    * Encodes itself to a sequence of redis messages that will be sent in one batch to the server.
    * Returns a [[RepliesDecoder]] responsible for translating raw Redis responses into the actual result.
    */
  def encodeCommands(messageBuffer: MessageBuffer, inTransaction: Boolean): RepliesDecoder[A]

  /**
    * Merges two batches into one. Provided function is applied on results of the batches being merged to obtain
    * result of the merged batch. Scope of the resulting batch is computed as least upper bound of the two scopes
    * being merged. For example, batch scoped with [[Scope.Node]] merged with a batch scoped with [[Scope.Cluster]]
    * will result in a batch scoped as [[Scope.Node]] (the batch contains at least one command that must be executed
    * on node client and not on cluster client; therefore merged batch also cannot be executed on cluster).
    */
  def map2[B, S0, C](other: RedisBatch[B, S0])(f: (A, B) => C): RedisBatch[C, S with S0] =
    new RedisBatch[C, S with S0] {
      def encodeCommands(messageBuffer: MessageBuffer, inTransaction: Boolean) = {
        val initialSize = messageBuffer.size
        val selfDecoder = self.encodeCommands(messageBuffer, inTransaction)
        val selfCount = messageBuffer.size - initialSize
        val otherDecoder = other.encodeCommands(messageBuffer, inTransaction)
        RepliesDecoder { (replies, start, end, state) =>
          val a = selfDecoder.decodeReplies(replies, start, start + selfCount, state)
          val b = otherDecoder.decodeReplies(replies, start + selfCount, end, state)
          f(a, b)
        }
      }
    }

  def <*[B, S0](other: RedisBatch[B, S0]): RedisBatch[A, S with S0] =
    map2(other)((a, _) => a)

  def *>[B, S0](other: RedisBatch[B, S0]): RedisBatch[B, S with S0] =
    map2(other)((_, b) => b)

  def zip[B, S0](other: RedisBatch[B, S0]): RedisBatch[(A, B), S with S0] =
    map2(other)((_, _))

  def map[B](f: A => B): RedisBatch[B, S] =
    new RedisBatch[B, S] {
      def encodeCommands(messageBuffer: MessageBuffer, inTransaction: Boolean) = {
        val decoder = self.encodeCommands(messageBuffer, inTransaction)
        RepliesDecoder((replies, start, end, state) => f(decoder.decodeReplies(replies, start, end, state)))
      }
    }

  /**
    * Returns a batch which invokes the same Redis commands as this batch, but ensures that
    * they are invoked inside a Redis transaction (`MULTI`-`EXEC`).
    */
  def transaction: AtomicBatch[A, S] =
    new Transaction(this)

  def atomic: AtomicBatch[A, S] =
    transaction
}

object RedisBatch extends HasFlatMap[OperationBatch] {
  implicit class NodeBatchOps[A](private val batch: OperationBatch[A]) extends AnyVal {
    def operation: RedisOp[A] = RedisOp.LeafOp(batch)
  }

  class Transaction[+A, -S](batch: RedisBatch[A, S]) extends AtomicBatch[A, S] {
    def encodeCommands(messageBuffer: MessageBuffer, inTransaction: Boolean) =
      if (inTransaction) {
        batch.encodeCommands(messageBuffer, inTransaction)
      } else {
        messageBuffer += Multi.encode
        val initialSize = messageBuffer.size
        val actualDecoder = batch.encodeCommands(messageBuffer, inTransaction = true)
        val actualSize = messageBuffer.size - initialSize
        messageBuffer += Exec.encode
        RepliesDecoder { (replies, start, end, state) =>
          state.watching = false
          replies(end - 1) match {
            case ArrayMsg(actualReplies) => actualDecoder.decodeReplies(actualReplies, 0, actualSize, state)
            case NullArrayMsg => throw new OptimisticLockException
            case errorMsg: ErrorMsg =>
              val actualErrors = replies.iterator.drop(start + 1).take(actualSize).map {
                case SimpleStringMsg(str) if str.utf8String == "QUEUED" => errorMsg
                case actualErrorMsg: ErrorMsg => actualErrorMsg
                case msg => throw new UnexpectedReplyException(s"expected simple string QUEUED or error, got $msg")
              }.to[ArrayBuffer]
              actualDecoder.decodeReplies(actualErrors, 0, actualSize, state)
            case _ => throw new UnexpectedReplyException("expected multi bulk reply")
          }
        }
      }

    override def transaction = this
  }

  class MessageBuffer(private val buffer: ArrayBuffer[ArrayMsg[BulkStringMsg]]) extends AnyVal {
    def +=(msg: ArrayMsg[BulkStringMsg]): Unit = buffer += msg
    def ++=(msgs: TraversableOnce[ArrayMsg[BulkStringMsg]]): Unit = buffer ++= msgs
    def size = buffer.size
  }

  final class ConnectionState {
    var watching: Boolean = false
  }

  trait RepliesDecoder[+A] {
    def decodeReplies(replies: IndexedSeq[RedisMsg], start: Int, end: Int, state: ConnectionState): A
  }
  object RepliesDecoder {
    def apply[A](f: (IndexedSeq[RedisMsg], Int, Int, ConnectionState) => A): RepliesDecoder[A] = Sam[RepliesDecoder[A]](f)
  }

  def success[A](a: A): RedisBatch[A, Any] =
    new RedisBatch[A, Any] with RedisBatch.RepliesDecoder[A] {
      def encodeCommands(messageBuffer: MessageBuffer, inTransaction: Boolean) = this
      def decodeReplies(replies: IndexedSeq[RedisMsg], start: Int, end: Int, state: ConnectionState) = a
    }

  def failure(cause: Throwable): RedisBatch[Nothing, Any] =
    new RedisBatch[Nothing, Any] with RedisBatch.RepliesDecoder[Nothing] {
      def encodeCommands(messageBuffer: MessageBuffer, inTransaction: Boolean) = this
      def decodeReplies(replies: IndexedSeq[RedisMsg], start: Int, end: Int, state: ConnectionState) = throw cause
    }

  val unit = success(())

  //TODO more API
  //TODO sequence
}

trait AtomicBatch[+A, -S] extends RedisBatch[A, S] {
  override def atomic: AtomicBatch[A, S] = this
}

/**
  * Represents a sequence of Redis operations executed using a single Redis connection.
  * Any operation may depend on the result of some previous operation (therefore, `flatMap` is available).
  */
sealed trait RedisOp[+A] {
  def map[B](f: A => B): RedisOp[B] =
    this.flatMap(a => RedisOp.success(f(a)))

  //TODO more API
}

object RedisOp extends HasFlatMap[RedisOp] {
  val unit: RedisOp[Unit] = success(())

  def success[A](a: A): RedisOp[A] =
    RedisBatch.success(a).operation

  def failure(cause: Throwable): RedisOp[Nothing] =
    RedisBatch.failure(cause).operation

  //TODO more API

  case class LeafOp[A](batch: OperationBatch[A]) extends RedisOp[A]
  case class FlatMappedOp[A, B](batch: OperationBatch[A], fun: A => RedisOp[B]) extends RedisOp[B]
}
