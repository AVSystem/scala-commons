package com.avsystem.commons
package redis

import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.protocol._

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
  * Represents a Redis operation or a set of operations which is sent to Redis as a single batch.
  *
  * @tparam A result yield by this batch
  */
trait RedisBatch[+A] {self =>

  import RedisBatch._

  def rawCommandPacks: RawCommandPacks
  def decodeReplies(replies: Int => RedisReply, index: Index = new Index, inTransaction: Boolean = false): A

  /**
    * Merges two batches into one. Provided function is applied on results of the batches being merged to obtain
    * result of the merged batch.
    */
  def map2[B, C](other: RedisBatch[B])(f: (A, B) => C): RedisBatch[C] =
  new RedisBatch[C] with RawCommandPacks {
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

  def transform[B](fun: Try[A] => Try[B]): RedisBatch[B] =
    new RedisBatch[B] {
      def rawCommandPacks = self.rawCommandPacks
      def decodeReplies(replies: Int => RedisReply, index: Index, inTransaction: Boolean) =
        fun(Try(self.decodeReplies(replies, index, inTransaction))).get
    }

  def recover[B >: A](f: PartialFunction[Throwable, B]): RedisBatch[B] =
    new RedisBatch[B] {
      def rawCommandPacks = self.rawCommandPacks
      def decodeReplies(replies: Int => RedisReply, index: Index, inTransaction: Boolean) =
        try self.decodeReplies(replies, index, inTransaction) catch f
    }

  def <*>[B, C](other: RedisBatch[B])(f: (A, B) => C): RedisBatch[C] =
    (self map2 other) (f)

  def <*[B](other: RedisBatch[B]): RedisBatch[A] =
    map2(other)((a, _) => a)

  def *>[B](other: RedisBatch[B]): RedisBatch[B] =
    map2(other)((_, b) => b)

  def zip[B](other: RedisBatch[B]): RedisBatch[(A, B)] =
    map2(other)((_, _))

  def map[B](f: A => B): RedisBatch[B] =
    new RedisBatch[B] {
      def rawCommandPacks = self.rawCommandPacks
      def decodeReplies(replies: Int => RedisReply, index: Index, inTransaction: Boolean) =
        f(self.decodeReplies(replies, index, inTransaction))
    }

  def failed: RedisBatch[Throwable] =
    transform {
      case Failure(t) => Success(t)
      case Success(_) => Failure(new NoSuchElementException("RedisBatch.failed not completed with a throwable"))
    }

  def tried: RedisBatch[Try[A]] =
    transform(Success(_))

  def ignoreFailures: RedisBatch[Unit] =
    transform(_ => Success(()))

  def operation: RedisOp[A] =
    RedisOp.LeafOp(this)

  def transaction: RedisBatch[A] =
    new Transaction(this)

  def atomic: RedisBatch[A] =
    transaction
}

object RedisBatch extends HasFlatMap[RedisBatch] {
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

  def success[A](a: A): RedisBatch[A] =
    new RedisBatch[A] with RawCommandPacks {
      def rawCommandPacks = this
      def emitCommandPacks(consumer: RawCommandPack => Unit) = ()
      def decodeReplies(replies: Int => RedisReply, index: Index, inTransaction: Boolean) = a
    }

  def failure(cause: Throwable): RedisBatch[Nothing] =
    new RedisBatch[Nothing] with RawCommandPacks {
      def rawCommandPacks = this
      def emitCommandPacks(consumer: RawCommandPack => Unit) = ()
      def decodeReplies(replies: Int => RedisReply, index: Index, inTransaction: Boolean) = throw cause
    }

  val unit = success(())

  implicit class SequenceOps[Ops, Res](private val ops: Ops) extends AnyVal {
    def sequence(implicit sequencer: Sequencer[Ops, Res]): RedisBatch[Res] = sequencer.sequence(ops)
  }

  //TODO more API
}

trait AtomicBatch[+A] extends RedisBatch[A] with RawCommandPack {
  def rawCommandPacks = this
  override def atomic: RedisBatch[A] = this
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

  case class LeafOp[A](batch: RedisBatch[A]) extends RedisOp[A]
  case class FlatMappedOp[A, B](batch: RedisBatch[A], fun: A => RedisOp[B]) extends RedisOp[B]
}
