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
trait Sequencer[Ops, Res, S] {
  def sequence(ops: Ops): RedisBatch[Res, S]
}
object Sequencer extends TupleSequencers {
  private val reusableTrivialSequencer = new Sequencer[RedisBatch[Any, Any], Any, Any] {
    def sequence(ops: RedisBatch[Any, Any]): RedisBatch[Any, Any] = ops
  }

  implicit def trivialSequencer[A, S]: Sequencer[RedisBatch[A, S], A, S] =
    reusableTrivialSequencer.asInstanceOf[Sequencer[RedisBatch[A, S], A, S]]

  implicit def collectionSequencer[A, S, M[X] <: TraversableOnce[X], That](
    implicit cbf: CanBuildFrom[M[RedisBatch[A, S]], A, That]): Sequencer[M[RedisBatch[A, S]], That, S] =

    new Sequencer[M[RedisBatch[A, S]], That, S] {
      def sequence(ops: M[RedisBatch[A, S]]) =
        new RedisBatch[That, S] with RawCommandPacks {
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
