package com.avsystem.commons
package redis

import com.avsystem.commons.redis.protocol.BulkStringMsg
import com.avsystem.commons.serialization.GenObjectCodec

import scala.annotation.implicitNotFound
import scala.collection.Factory
import scala.collection.immutable.ArraySeq

@implicitNotFound("${T} has no RedisRecordCodec. It can be derived from GenObjectCodec which can be provided " +
  "by making your case class companion extend HasGenObjectCodec")
case class RedisRecordCodec[T](read: IndexedSeq[BulkStringMsg] => T, write: T => IndexedSeq[BulkStringMsg])
object RedisRecordCodec extends LowPriorityRedisRecordCodecs {
  def apply[T](implicit codec: RedisRecordCodec[T]): RedisRecordCodec[T] = codec

  implicit def forDataMap[M[X, Y] <: BMap[X, Y], F: RedisDataCodec, V: RedisDataCodec](
    implicit fac: Factory[(F, V), M[F, V]]
  ): RedisRecordCodec[M[F, V] with BMap[F, V]] =
    RedisRecordCodec(elems => record[F, V, M[F, V]](elems), map => bulks(map.iterator, map.size))

  implicit def forDataSeq[M[X] <: Seq[X], F: RedisDataCodec, V: RedisDataCodec](
    implicit fac: Factory[(F, V), M[(F, V)]]
  ): RedisRecordCodec[M[(F, V)] with Seq[(F, V)]] =
    RedisRecordCodec(elems => record[F, V, M[(F, V)]](elems), seq => bulks(seq.iterator, seq.size))

  private def record[F: RedisDataCodec, V: RedisDataCodec, To](
    elems: IndexedSeq[BulkStringMsg])(implicit fac: Factory[(F, V), To]
  ): To = {
    val b = fac.newBuilder
    b.sizeHint(elems.size)
    elems.iterator.pairs.foreach {
      case (BulkStringMsg(f), BulkStringMsg(v)) =>
        b += RedisDataCodec[F].read(f) -> RedisDataCodec[V].read(v)
    }
    b.result()
  }

  private def bulks[F: RedisDataCodec, V: RedisDataCodec](it: Iterator[(F, V)], size: Int): IndexedSeq[BulkStringMsg] =
    it.flatMap { case (f, v) => List(RedisDataCodec.write(f), RedisDataCodec.write(v)) }
      .map(BulkStringMsg).toSized(ArraySeq, size)
}
sealed trait LowPriorityRedisRecordCodecs { this: RedisRecordCodec.type =>
  implicit def fromApplyUnapplyCodec[T](implicit codec: GenObjectCodec[T]): RedisRecordCodec[T] =
    RedisRecordCodec(
      elems => codec.readObject(new RedisRecordInput(elems)),
      value => {
        val builder = ArraySeq.newBuilder[BulkStringMsg]
        codec.writeObject(new RedisRecordOutput(builder), value)
        builder.result()
      }
    )
}
