package com.avsystem.commons
package redis

import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.RedisBatch.Index
import com.avsystem.commons.redis.protocol.RedisReply

import scala.annotation.implicitNotFound
import scala.collection.generic.CanBuildFrom
import scala.util.control.NonFatal

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
  * In order to perform "sequencing", simply call `sequence` on your collection of batches, e.g.
  *
  * {{{
  *   import akka.util.ByteString
  *   import com.avsystem.commons.redis.RedisCommands._
  *
  *   val key1 = ByteString("key1")
  *   val key2 = ByteString("key2")
  *
  *   val tupleBatch: RedisBatch[(Long, String)] = (incr(key1), get(key2).map(_.utf8String)).sequence
  *   val listBatch: RedisBatch[List[Int]] = List(key1, key2).map(key => incr(key)).sequence
  * }}}
  */
@implicitNotFound("${Ops} is not something that can be transformed into RedisBatch")
trait Sequencer[Ops, Res] {
  def sequence(ops: Ops): RedisBatch[Res]
}
object Sequencer extends TupleSequencers {
  private val reusableTrivialSequencer = new Sequencer[RedisBatch[Any], Any] {
    def sequence(ops: RedisBatch[Any]): RedisBatch[Any] = ops
  }

  implicit def trivialSequencer[A]: Sequencer[RedisBatch[A], A] =
    reusableTrivialSequencer.asInstanceOf[Sequencer[RedisBatch[A], A]]

  implicit def collectionSequencer[ElOps, ElRes, M[X] <: TraversableOnce[X], That](
    implicit elSequencer: Sequencer[ElOps, ElRes], cbf: CanBuildFrom[M[ElOps], ElRes, That]): Sequencer[M[ElOps], That] =

    new Sequencer[M[ElOps], That] {
      def sequence(ops: M[ElOps]) =
        new RedisBatch[That] with RawCommandPacks {
          def rawCommandPacks = this
          def emitCommandPacks(consumer: RawCommandPack => Unit) =
            ops.foreach(e => elSequencer.sequence(e).rawCommandPacks.emitCommandPacks(consumer))
          def decodeReplies(replies: Int => RedisReply, index: Index, inTransaction: Boolean) = {
            // we must invoke all decoders regardless of intermediate errors because index must be properly advanced
            var failure: Opt[Throwable] = Opt.Empty
            val builder = cbf(ops)
            ops.foreach { batch =>
              try {
                builder += elSequencer.sequence(batch).decodeReplies(replies, index, inTransaction)
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
    }
}
