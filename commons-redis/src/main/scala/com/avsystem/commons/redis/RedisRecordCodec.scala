package com.avsystem.commons
package redis

import com.avsystem.commons.redis.protocol.BulkStringMsg
import com.avsystem.commons.serialization.ApplyUnapplyCodec

import scala.collection.generic.CanBuildFrom

case class RedisRecordCodec[T](read: IndexedSeq[BulkStringMsg] => T, write: T => IndexedSeq[BulkStringMsg])
object RedisRecordCodec {
  implicit def fromApplyUnapplyCodec[T](implicit codec: ApplyUnapplyCodec[T]): RedisRecordCodec[T] =
    RedisRecordCodec(
      elems => codec.readObject(new RedisRecordInput(elems)),
      value => {
        val result = new MArrayBuffer[BulkStringMsg]
        codec.writeObject(new RedisRecordOutput(result), value)
        result
      }
    )

  implicit def forDataMap[M[X, Y] <: BMap[X, Y], F: RedisDataCodec, V: RedisDataCodec](
    implicit cbf: CanBuildFrom[Nothing, (F, V), M[F, V]]
  ): RedisRecordCodec[M[F, V] with BMap[F, V]] =
    RedisRecordCodec(elems => record[F, V, M[F, V]](elems), map => bulks(map.iterator, map.size))

  implicit def forDataSeq[M[X] <: Seq[X], F: RedisDataCodec, V: RedisDataCodec](
    implicit cbf: CanBuildFrom[Nothing, (F, V), M[(F, V)]]
  ): RedisRecordCodec[M[(F, V)] with Seq[(F, V)]] =
    RedisRecordCodec(elems => record[F, V, M[(F, V)]](elems), seq => bulks(seq.iterator, seq.size))

  private def record[F: RedisDataCodec, V: RedisDataCodec, To](
    elems: IndexedSeq[BulkStringMsg])(implicit cbf: CanBuildFrom[Nothing, (F, V), To]
  ): To = {
    val b = cbf()
    b.sizeHint(elems.size)
    elems.iterator.pairs.foreach {
      case (BulkStringMsg(f), BulkStringMsg(v)) =>
        b += RedisDataCodec[F].read(f) -> RedisDataCodec[V].read(v)
    }
    b.result()
  }

  private def bulks[F: RedisDataCodec, V: RedisDataCodec](it: Iterator[(F, V)], size: Int): IndexedSeq[BulkStringMsg] =
    it.flatMap { case (f, v) => List(RedisDataCodec.write(f), RedisDataCodec.write(v)) }
      .map(BulkStringMsg).toSized[MArrayBuffer](size)
}
