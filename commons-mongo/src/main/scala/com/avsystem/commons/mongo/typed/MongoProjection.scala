package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.mongo.mongoId
import org.bson.BsonDocument

import scala.annotation.tailrec

trait MongoProjection[E, T] {
  def projectionRefs: Set[MongoRef[E, _]]
  def decodeFrom(doc: BsonDocument): T
  def showRecordId: Boolean

  def withRecordId: MongoProjection[E, WithRecordId[T]] =
    MongoProjection.ShowRecordId(this)

  final def toProjectionBson: BsonDocument = {
    val doc = new BsonDocument
    @tailrec def loop(it: Iterator[MongoRef[E, _]]): Unit =
      if (it.hasNext) it.next() match {
        case propRef: MongoPropertyRef[E, _] =>
          doc.put(propRef.projectionPath, Bson.int(1))
          loop(it)
        case _: MongoDataRef[E, _] =>
          doc.clear()
      }
    loop(projectionRefs.iterator)
    if (!doc.isEmpty && !doc.containsKey(mongoId.Id)) {
      doc.put(mongoId.Id, Bson.int(0))
    }
    doc
  }

  final def map[T0](fun: T => T0): MongoProjection[E, T0] =
    MongoProjection.Mapped(this, fun)

  final def map2[T0, T1](other: MongoProjection[E, T0])(fun: (T, T0) => T1): MongoProjection[E, T1] =
    MongoProjection.Composed(this, other, fun)

  final def zip[T0](other: MongoProjection[E, T0]): MongoProjection[E, (T, T0)] =
    map2(other)((_, _))
}

object MongoProjection extends ProjectionZippers {
  final class Empty[E]() extends MongoProjection[E, Unit] {
    def projectionRefs: Set[MongoRef[E, _]] = Set.empty
    def showRecordId: Boolean = false
    def decodeFrom(doc: BsonDocument): Unit = ()
  }

  final case class Mapped[E, T, T0](prev: MongoProjection[E, T], fun: T => T0) extends MongoProjection[E, T0] {
    def projectionRefs: Set[MongoRef[E, _]] = prev.projectionRefs
    def showRecordId: Boolean = prev.showRecordId
    def decodeFrom(doc: BsonDocument): T0 = fun(prev.decodeFrom(doc))
  }

  final case class Composed[E, T, T0, T1](
    first: MongoProjection[E, T], second: MongoProjection[E, T0], fun: (T, T0) => T1
  ) extends MongoProjection[E, T1] {
    def projectionRefs: Set[MongoRef[E, _]] =
      first.projectionRefs ++ second.projectionRefs

    def showRecordId: Boolean = first.showRecordId || second.showRecordId

    def decodeFrom(doc: BsonDocument): T1 =
      fun(first.decodeFrom(doc), second.decodeFrom(doc))
  }

  final val RecordId = "$recordId"

  final case class ShowRecordId[E, T](projection: MongoProjection[E, T]) extends MongoProjection[E, WithRecordId[T]] {
    def projectionRefs: Set[MongoRef[E, _]] = projection.projectionRefs

    def decodeFrom(doc: BsonDocument): WithRecordId[T] =
      WithRecordId(projection.decodeFrom(doc), doc.getInt64(RecordId).getValue)

    def showRecordId: Boolean = true
  }
}

case class WithRecordId[T](data: T, recordId: Long)
