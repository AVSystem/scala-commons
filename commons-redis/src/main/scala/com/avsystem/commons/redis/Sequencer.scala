package com.avsystem.commons
package redis

import com.avsystem.commons.redis.RedisBatch.Index
import com.avsystem.commons.redis.protocol.RedisReply

import scala.annotation.implicitNotFound
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Typeclass for easy merging ("sequencing") of multiple [[RedisBatch]] instances into one. This is done in
  * order to guarantee that some set of operations is sent to Redis as a single batch (most likely single network
  * message).
  *
  * The parameter `Ops` represents batches that will be sequenced into one. It may be a collection, a tuple or
  * any other "collection-like" type for which type class instance is provided.
  *
  * `Res` is the type of result of the batch created by "sequencing". This type is automatically inferred from
  * the `Ops` type. For example, if `Ops` is `(RedisBatch[Int], RedisBatch[String])` (tuple) then `Res` will be
  * `(Int, String)`. If `Ops` is `List[RedisBatch[Int&#93;]` then `Res` will be `List[Int]`.
  *
  * Nesting is also possible. For example, if `Ops` is `(List[RedisBatch[Int&#93;], RedisBatch[String])` then
  * `Res` will be `(List[Int], String)`.
  *
  * In order to perform "sequencing", simply call [[RedisBatch.SequenceOps#sequence sequence]]
  * on your collection of batches, e.g.
  *
  * {{{
  *   import RedisApi.Batches.StringTyped._
  *
  *   val tupleBatch: RedisBatch[(Long, Opt[String])] = (incr("key1"), get("key2")).sequence
  *   val listBatch: RedisBatch[List[Long]] = List("key1", "key2").map(key => incr(key)).sequence
  * }}}
  */
@implicitNotFound("${Ops} is not something that can be transformed into RedisBatch")
trait Sequencer[Ops, Res] {
  def sequence(ops: Ops): RedisBatch[Res]
}
object Sequencer extends TupleSequencers {
  private val reusableTrivialSequencer = new Sequencer[RedisBatch[Any], Any] with (RedisBatch[Any] => RedisBatch[Any]) {
    def sequence(ops: RedisBatch[Any]): RedisBatch[Any] = ops
    def apply(ops: RedisBatch[Any]): RedisBatch[Any] = ops
  }

  implicit def trivialSequencer[A]: Sequencer[RedisBatch[A], A] =
    reusableTrivialSequencer.asInstanceOf[Sequencer[RedisBatch[A], A]]

  implicit def collectionSequencer[ElOps, ElRes, M[X] <: TraversableOnce[X], That](
    implicit elSequencer: Sequencer[ElOps, ElRes], cbf: CanBuildFrom[M[ElOps], ElRes, That]): Sequencer[M[ElOps], That] =

    new Sequencer[M[ElOps], That] {
      def sequence(ops: M[ElOps]): CollectionBatch[ElRes, That] = {
        val batches: Traversable[RedisBatch[ElRes]] =
          if ((elSequencer eq reusableTrivialSequencer) && ops.isInstanceOf[Traversable[Any]])
            ops.asInstanceOf[Traversable[RedisBatch[ElRes]]]
          else {
            val buf = new ArrayBuffer[RedisBatch[ElRes]]
            ops.foreach(el => buf += elSequencer.sequence(el))
            buf
          }
        new CollectionBatch[ElRes, That](batches, () => cbf(ops))
      }
    }
}

final class CollectionBatch[A, C](batches: Traversable[RedisBatch[A]], builderCreator: () => mutable.Builder[A, C])
  extends RedisBatch[C] with RawCommandPacks {

  def rawCommandPacks: CollectionBatch[A, C] = this
  def emitCommandPacks(consumer: RawCommandPack => Unit): Unit =
    batches.foreach(_.rawCommandPacks.emitCommandPacks(consumer))
  def computeSize(limit: Int): Int =
    if (limit <= 0) limit else batches.foldLeft(0)((s, b) => s + b.rawCommandPacks.computeSize(limit - s))
  def decodeReplies(replies: Int => RedisReply, index: Index, inTransaction: Boolean): C = {
    // we must invoke all decoders regardless of intermediate errors because index must be properly advanced
    var failure: Opt[Throwable] = Opt.Empty
    val builder = builderCreator()
    batches.foreach { batch =>
      try {
        builder += batch.decodeReplies(replies, index, inTransaction)
      } catch {
        case NonFatal(cause) =>
          failure = failure orElse cause.opt
      }
    }
    failure match {
      case Opt.Empty => builder.result()
      case Opt(cause) => throw cause
    }
  }
}
