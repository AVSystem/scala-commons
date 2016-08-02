package com.avsystem.commons
package redis

import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.RedisBatch.Index
import com.avsystem.commons.redis.protocol.RedisReply

import scala.annotation.implicitNotFound
import scala.collection.generic.CanBuildFrom
import scala.util.control.NonFatal

/**
  * Author: ghik
  * Created: 28/07/16.
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

  implicit def collectionSequencer[A, M[X] <: TraversableOnce[X], That](
    implicit cbf: CanBuildFrom[M[RedisBatch[A]], A, That]): Sequencer[M[RedisBatch[A]], That] =

    new Sequencer[M[RedisBatch[A]], That] {
      def sequence(ops: M[RedisBatch[A]]) =
        new RedisBatch[That] with RawCommandPacks {
          def rawCommandPacks = this
          def emitCommandPacks(consumer: RawCommandPack => Unit) =
            ops.foreach(_.rawCommandPacks.emitCommandPacks(consumer))
          def decodeReplies(replies: Int => RedisReply, index: Index, inTransaction: Boolean) = {
            // we must invoke all decoders regardless of intermediate errors because index must be properly advanced
            var failure: Opt[Throwable] = Opt.Empty
            val builder = cbf(ops)
            ops.foreach { batch =>
              try {
                builder += batch.decodeReplies(replies, index, inTransaction)
              } catch {
                case NonFatal(cause) =>
                  if (failure.isEmpty) {
                    failure = cause.opt
                  }
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
