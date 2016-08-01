package com.avsystem.commons
package redis

import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.protocol._

import scala.util.control.NonFatal

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

  import RedisBatch._

  def rawCommandPacks: RawCommandPacks
  def decodeReplies(replies: Int => RedisReply, index: Index = new Index, inTransaction: Boolean = false): A

  /**
    * Merges two batches into one. Provided function is applied on results of the batches being merged to obtain
    * result of the merged batch. Scope of the resulting batch is computed as least upper bound of the two scopes
    * being merged. For example, batch scoped with [[Scope.Node]] merged with a batch scoped with [[Scope.Cluster]]
    * will result in a batch scoped as [[Scope.Node]] (the batch contains at least one command that must be executed
    * on node client and not on cluster client; therefore merged batch also cannot be executed on cluster).
    */
  def map2[B, S0, C](other: RedisBatch[B, S0])(f: (A, B) => C): RedisBatch[C, S with S0] =
  new RedisBatch[C, S with S0] with RawCommandPacks {
    def rawCommandPacks = this

    def emitCommandPacks(consumer: RawCommandPack => Unit) = {
      self.rawCommandPacks.emitCommandPacks(consumer)
      other.rawCommandPacks.emitCommandPacks(consumer)
    }

    def decodeReplies(replies: Int => RedisReply, index: Index, inTransaction: Boolean) = {
      // we must invoke both decoders regardless of intermediate errors because index must be properly advanced
      var failure: Opt[Throwable] = Opt.Empty
      val a = try self.decodeReplies(replies, index, inTransaction) catch {
        case NonFatal(cause) =>
          if (failure.isEmpty) {
            failure = cause.opt
          }
          null.asInstanceOf[A]
      }
      val b = try other.decodeReplies(replies, index, inTransaction) catch {
        case NonFatal(cause) =>
          if (failure.isEmpty) {
            failure = cause.opt
          }
          null.asInstanceOf[B]
      }
      failure match {
        case Opt.Empty => f(a, b)
        case Opt(cause) => throw cause
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
    new RedisBatch[B, S] with RawCommandPacks {
      def rawCommandPacks = this
      def emitCommandPacks(consumer: RawCommandPack => Unit) =
        self.rawCommandPacks.emitCommandPacks(consumer)
      def decodeReplies(replies: Int => RedisReply, index: Index, inTransaction: Boolean) =
        f(self.decodeReplies(replies, index, inTransaction))
    }

  /**
    * Returns a batch which invokes the same Redis commands as this batch, but ensures that
    * they are invoked inside a Redis transaction (`MULTI`-`EXEC`).
    */
  def transaction: RedisBatch[A, S] =
  new Transaction(this)

  def atomic: RedisBatch[A, S] =
    transaction
}

object RedisBatch extends HasFlatMap[OperationBatch] {
  implicit class NodeBatchOps[A](private val batch: OperationBatch[A]) extends AnyVal {
    def operation: RedisOp[A] = RedisOp.LeafOp(batch)
  }

  final class Index {
    var value: Int = 0
    def inc(): Int = {
      val res = value
      value += 1
      res
    }
  }
  object Index {
    def apply(value: Int): Index = {
      val res = new Index
      res.value = value
      res
    }
  }

  def success[A](a: A): RedisBatch[A, Any] =
    new RedisBatch[A, Any] with RawCommandPacks {
      def rawCommandPacks = this
      def emitCommandPacks(consumer: RawCommandPack => Unit) = ()
      def decodeReplies(replies: Int => RedisReply, index: Index, inTransaction: Boolean) = a
    }

  def failure(cause: Throwable): RedisBatch[Nothing, Any] =
    new RedisBatch[Nothing, Any] with RawCommandPacks {
      def rawCommandPacks = this
      def emitCommandPacks(consumer: RawCommandPack => Unit) = ()
      def decodeReplies(replies: Int => RedisReply, index: Index, inTransaction: Boolean) = throw cause
    }

  val unit = success(())

  implicit class SequenceOps[Ops, Res, S](private val ops: Ops) extends AnyVal {
    def sequence(implicit sequencer: Sequencer[Ops, Res, S]): RedisBatch[Res, S] = sequencer.sequence(ops)
  }

  //TODO more API
}

trait AtomicBatch[+A, -S] extends RedisBatch[A, S] with RawCommandPack {
  def rawCommandPacks = this
  override def atomic: RedisBatch[A, S] = this
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
