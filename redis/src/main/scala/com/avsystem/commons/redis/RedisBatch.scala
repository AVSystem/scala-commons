package com.avsystem.commons
package redis

import com.avsystem.commons.redis.RedisOp.{FlatMappedOp, LeafOp}
import com.avsystem.commons.redis.protocol._
import com.avsystem.commons.redis.util.FoldingBuilder

import scala.collection.BuildFrom
import scala.collection.mutable.ArrayBuffer

/** Represents a Redis command or a set of commands sent to Redis as a single batch (usually in a single network
  * message). [[RedisBatch]] yields a result of type `A` which is decoded from responses to commands from this batch.
  * Execution of a batch may also fail for various reasons. Therefore, [[RedisBatch]] contains API that allows to
  * recover from failures, e.g. [[RedisBatch.tried]].
  *
  * [[RedisBatch]]es may be combined with each other to form bigger batches. The simpliest primitive for combining
  * batches is [[RedisBatch.map2]] operation while the most convenient way is by using [[RedisBatch.sequence]] operation
  * which is powered by [[Sequencer]] typeclass.
  *
  * [[RedisBatch]] can be turned into a [[RedisOp]] or executed by [[RedisExecutor]] (e.g. one of Redis client
  * implementations).
  *
  * @tparam A
  *   result yield by this batch
  */
trait RedisBatch[+A] { self =>

  import RedisBatch._

  def rawCommandPacks: RawCommandPacks
  def decodeReplies(replies: Int => RedisReply, index: Index = new Index, inTransaction: Boolean = false): A

  /** Merges two batches into one. Provided function is applied on results of the batches being merged to obtain result
    * of the merged batch. `map2` is the fundamental primitive for composing multiple batches into one.
    */
  def map2[B, C](other: RedisBatch[B])(f: (A, B) => C): RedisBatch[C] =
    new RedisBatch[C] with RawCommandPacks {
      def rawCommandPacks: RawCommandPacks = this

      def emitCommandPacks(consumer: RawCommandPack => Unit): Unit = {
        self.rawCommandPacks.emitCommandPacks(consumer)
        other.rawCommandPacks.emitCommandPacks(consumer)
      }

      def computeSize(limit: Int): Int =
        if (limit <= 0) limit
        else {
          val selfSize = self.rawCommandPacks.computeSize(limit)
          selfSize + other.rawCommandPacks.computeSize(limit - selfSize)
        }

      def decodeReplies(replies: Int => RedisReply, index: Index, inTransaction: Boolean): C = {
        // we must invoke both decoders regardless of intermediate errors because index must be properly advanced
        var failure: Opt[Throwable] = Opt.Empty
        val a =
          try self.decodeReplies(replies, index, inTransaction)
          catch {
            case NonFatal(cause) =>
              failure = failure orElse cause.opt
              null.asInstanceOf[A]
          }
        val b =
          try other.decodeReplies(replies, index, inTransaction)
          catch {
            case NonFatal(cause) =>
              failure = failure orElse cause.opt
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
      def rawCommandPacks: RawCommandPacks = self.rawCommandPacks
      def decodeReplies(replies: Int => RedisReply, index: Index, inTransaction: Boolean): B =
        fun(Try(self.decodeReplies(replies, index, inTransaction))).get
    }

  def recover[B >: A](f: PartialFunction[Throwable, B]): RedisBatch[B] =
    new RedisBatch[B] {
      def rawCommandPacks: RawCommandPacks = self.rawCommandPacks
      def decodeReplies(replies: Int => RedisReply, index: Index, inTransaction: Boolean): B =
        try self.decodeReplies(replies, index, inTransaction)
        catch f
    }

  /** This is a symbolic alias for [[map2]]. The symbol (along with [[*>]] and [[<*]]) is inspired by its
    * [[http://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Applicative.html#v:-60--42--62- Haskell equivalent]].
    */
  def <*>[B, C](other: RedisBatch[B])(f: (A, B) => C): RedisBatch[C] =
    self.map2(other)(f)

  /** Merges two batches into a single batch where result of the left-hand-side batch is returned while result of
    * right-hand-side is discarded. Useful when right-hand-side returns `Unit`. NOTE: errors for right-hand-side are NOT
    * discarded, use [[ignoreFailures]] on it if that's your intention.
    */
  def <*[B](other: RedisBatch[B]): RedisBatch[A] =
    map2(other)((a, _) => a)

  /** Merges two batches into a single batch where result of the right-hand-side batch is returned while result of
    * left-hand-side is discarded. Useful when left-hand-side returns `Unit`. NOTE: errors for left-hand-side are NOT
    * discarded, use [[ignoreFailures]] on it if that's your intention.
    */
  def *>[B](other: RedisBatch[B]): RedisBatch[B] =
    map2(other)((_, b) => b)

  def zip[B](other: RedisBatch[B]): RedisBatch[(A, B)] =
    map2(other)((_, _))

  def map[B](f: A => B): RedisBatch[B] =
    new RedisBatch[B] {
      def rawCommandPacks: RawCommandPacks = self.rawCommandPacks
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

  /** Transforms this batch into a [[RedisOp]].
    */
  def operation: RedisOp[A] =
    RedisOp.LeafOp(this)

  /** Wraps this batch into a Redis transaction, i.e. ensures that it's executed inside a `MULTI`-`EXEC` block. NOTE: If
    * you simply want to ensure atomicity, use [[atomic]]. NOTE: You can safely nest transactions, the driver will make
    * sure that there are no nested `MULTI`-`EXEC` blocks on the wire.
    */
  def transaction: RedisBatch[A] =
    new Transaction(this)

  /** Returns a batch which invokes the same commands as this batch but atomically. If this batch is already atomic then
    * it's returned unchanged. Otherwise, it's wrapped into a Redis [[transaction]] (`MULTI`-`EXEC` block). Empty
    * batches, single-command batches and transactions are atomic by themselves and therefore are returned unchanged.
    */
  def atomic: RedisBatch[A] =
    if (rawCommandPacks.computeSize(2) > 1) transaction else this

  /** Ensures that every keyed command in this batch is prepended with `ASKING` special command. This is necessary only
    * when manually handling Redis Cluster redirections.
    */
  def asking: RedisBatch[A] =
    new RedisBatch[A] with RawCommandPacks {
      def decodeReplies(replies: Int => RedisReply, index: Index, inTransaction: Boolean): A =
        self.decodeReplies(replies, index, inTransaction)
      def emitCommandPacks(consumer: RawCommandPack => Unit): Unit =
        self.rawCommandPacks.emitCommandPacks(pack => consumer(new RedisClusterClient.AskingPack(pack)))
      def computeSize(limit: Int): Int =
        self.rawCommandPacks.computeSize(limit)
      def rawCommandPacks: RawCommandPacks = this
    }
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
    new ImmediateBatch[A] {
      def result: A = a
    }

  def failure(cause: Throwable): RedisBatch[Nothing] =
    new ImmediateBatch[Nothing] {
      def result: Nothing = throw cause
    }

  def fromTry[T](t: Try[T]): RedisBatch[T] = t match {
    case Success(value) => success(value)
    case Failure(cause) => failure(cause)
  }

  final val unit: RedisBatch[Unit] = success(())

  implicit class SequenceOps[Ops, Res](private val ops: Ops) extends AnyVal {

    /** Merges multiple [[RedisBatch]]es into one. Alternative syntax for [[RedisBatch.sequence]]. See [[Sequencer]] for
      * more general description of sequencing.
      *
      * Example usage:
      * {{{
      *   import RedisApi.Batches.StringTyped._
      *
      *   // tuple of batches -> single batch of a tuple
      *   val tupleBatch: RedisBatch[(Opt[String],Long)] =
      *     (get("key1"), incr("key2")).sequence
      *
      *   // collection of batches -> single batch of a collection
      *   val seqBatch: RedisBatch[Seq[Opt[String]]] =
      *     (1 to 10).map(i => get(s"key$$i")).sequence
      *
      *   // collection of tuples of batches -> single batch of collection of tuples
      *   val tupleCollectionBatch: RedisBatch[Seq[(Opt[String], Long)]] =
      *     (1 to 10).map(i => (get(s"stringKey$$i"), incr(s"numberKey$$i"))).sequence
      * }}}
      */
    def sequence(implicit sequencer: Sequencer[Ops, Res]): RedisBatch[Res] = sequencer.sequence(ops)
  }

  /** Merges multiple [[RedisBatch]]es into one. This is similar to Scala's `Future.sequence` but more general, because
    * the left-hand-side (`Ops`) can be more than just a collection. It can be any type for which an instance of
    * [[Sequencer]] type-function is defined. This includes all standard Scala collections and tuples. See [[Sequencer]]
    * for more details.
    *
    * Example usage:
    * {{{
    *   import RedisApi.Batches.StringTyped._
    *
    *   // tuple of batches -> single batch of a tuple
    *   val tupleBatch: RedisBatch[(Opt[String],Long)] =
    *     RedisBatch.sequence(get("key1"), incr("key2"))
    *
    *   // collection of batches -> single batch of a collection
    *   val seqBatch: RedisBatch[Seq[Opt[String]]] =
    *     RedisBatch.sequence((1 to 10).map(i => get(s"key$$i")))
    *
    *   // collection of tuples of batches -> single batch of collection of tuples
    *   val tupleCollectionBatch: RedisBatch[Seq[(Opt[String], Long)]] =
    *     RedisBatch.sequence((1 to 10).map(i => (get(s"stringKey$$i"), incr(s"numberKey$$i"))))
    * }}}
    */
  def sequence[Ops, Res](ops: Ops)(implicit sequencer: Sequencer[Ops, Res]): RedisBatch[Res] =
    sequencer.sequence(ops)

  def traverse[M[X] <: IterableOnce[X], A, B, That](
    coll: M[A]
  )(
    opFun: A => RedisBatch[B]
  )(implicit bf: BuildFrom[M[A], B, That]
  ): RedisBatch[That] = {
    val batches = new ArrayBuffer[RedisBatch[B]]
    coll.iterator.foreach(a => batches += opFun(a))
    new CollectionBatch[B, That](batches, () => bf.newBuilder(coll))
  }

  def foldLeftMap[A, B, T](coll: IterableOnce[A], zero: T)(opFun: A => RedisBatch[B])(fun: (T, B) => T): RedisBatch[T] = {
    val batches = new ArrayBuffer[RedisBatch[B]]
    coll.iterator.foreach(a => batches += opFun(a))
    new CollectionBatch[B, T](batches, () => new FoldingBuilder(zero, fun))
  }

  def foldLeft[T, A](ops: IterableOnce[RedisBatch[A]], zero: T)(fun: (T, A) => T): RedisBatch[T] =
    foldLeftMap(ops, zero)(identity)(fun)

  def foreach[A](ops: IterableOnce[A])(opFun: A => RedisBatch[Any]): RedisBatch[Unit] =
    foldLeftMap(ops, ())(opFun)((_, _) => ())
}

trait SinglePackBatch[+A] extends RedisBatch[A] with RawCommandPack {
  final def rawCommandPacks: RawCommandPacks = this
}

trait ImmediateBatch[+A] extends RedisBatch[A] with RawCommandPacks {
  def result: A
  final def rawCommandPacks: RawCommandPacks = this
  final def decodeReplies(replies: Int => RedisReply, index: RedisBatch.Index, inTransaction: Boolean): A = result
  final def emitCommandPacks(consumer: RawCommandPack => Unit): Unit = ()
  final def computeSize(limit: Int): Int = 0
  override final def maxBlockingMillis: Int = 0
}

/** Represents a sequence of Redis operations executed using a single Redis connection. Any operation may depend on the
  * result of some previous operation (therefore, `flatMap` is available). [[RedisOp]] is guaranteed to be executed
  * fully and exclusively on a single Redis connection (no other concurrent commands can be executed on that connection
  * during execution of [[RedisOp]]). Because of that, [[RedisOp]]s may execute `WATCH` and `UNWATCH` commands.
  *
  * In fact, the primary purpose of [[RedisOp]] is to allow execution of Redis transactions with optimistic locking. For
  * this purpose, [[RedisOp]] may be created by flat-mapping [[RedisBatch]]es.
  *
  * For example, below is an implementation of Redis transaction which fetches a value of some key (as `Int`) multiplies
  * it by 3 and saves it back to Redis. During this operation, the key being modified is watched so that saving fails
  * with [[exception.OptimisticLockException OptimisticLockException]] if some other client concurrently modifies that
  * key.
  *
  * {{{
  * val api = RedisApi.Batches.StringTyped.valueType[Int]
  * import api._
  * val transactionOp: RedisOp[Unit] = for {
  *   // we're sending WATCH and GET commands in a single batch
  *   value <- watch("number") *> get("number").map(_.getOrElse(1))
  *   // SET command is wrapped in MULTI-EXEC block
  *   _ <- set("number", value * 3).transaction
  * } yield ()
  * }}}
  *
  * [[RedisOp]] can be passed for execution to [[RedisOpExecutor]] (implemented by e.g. [[RedisNodeClient]]).
  */
sealed trait RedisOp[+A] { self =>
  def map[B](f: A => B): RedisOp[B] =
    self.flatMap(a => RedisOp.success(f(a)))

  def transform[B](fun: Try[A] => Try[B]): RedisOp[B] =
    self match {
      case LeafOp(batch) => LeafOp(batch.transform(fun))
      case FlatMappedOp(batch, pfun) => FlatMappedOp(batch, pfun.andThen(_.transform(fun)))
    }

  def recover[B >: A](f: PartialFunction[Throwable, B]): RedisOp[B] =
    self match {
      case LeafOp(batch) => LeafOp(batch.recover(f))
      case FlatMappedOp(batch, pfun) => FlatMappedOp(batch, pfun.andThen(_.recover(f)))
    }

  def recoverWith[B >: A](fun: PartialFunction[Throwable, RedisOp[B]]): RedisOp[B] =
    transform {
      case Failure(cause) => Success(fun.applyOrElse(cause, (t: Throwable) => RedisOp.failure(t)))
      case Success(value) => Success(RedisOp.success(value))
    }.flatMap(identity)

  def fallbackTo[B >: A](op: RedisOp[B]): RedisOp[B] =
    recoverWith { case _ => op }

  def failed: RedisOp[Throwable] =
    transform {
      case Failure(cause) => Success(cause)
      case Success(_) => Failure(new NoSuchElementException("RedisOp.failed not completed with a throwable"))
    }

  def tried: RedisOp[Try[A]] =
    transform(Success(_))

  def ignoreFailures: RedisOp[Unit] =
    transform(_ => Success(()))

  /** Ensures that every keyed command in this operation is prepended with special `ASKING` command. This is necessary
    * only when manually handling Redis Cluster redirections.
    */
  def asking: RedisOp[A] = self match {
    case LeafOp(batch) => LeafOp(batch.asking)
    case FlatMappedOp(batch, fun) => FlatMappedOp(batch.asking, fun.andThen(_.asking))
  }
}

object RedisOp extends HasFlatMap[RedisOp] {
  val unit: RedisOp[Unit] = success(())

  def success[A](a: A): RedisOp[A] =
    RedisBatch.success(a).operation

  def failure(cause: Throwable): RedisOp[Nothing] =
    RedisBatch.failure(cause).operation

  def fromTry[T](t: Try[T]): RedisOp[T] = t match {
    case Success(value) => success(value)
    case Failure(cause) => failure(cause)
  }

  case class LeafOp[A](batch: RedisBatch[A]) extends RedisOp[A]
  case class FlatMappedOp[A, B](batch: RedisBatch[A], fun: A => RedisOp[B]) extends RedisOp[B]
}
