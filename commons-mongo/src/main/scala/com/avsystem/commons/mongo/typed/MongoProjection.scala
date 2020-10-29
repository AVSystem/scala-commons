package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.mongo.mongoId
import org.bson.BsonDocument

trait MongoProjection[E, T] {
  def impliedFilter: MongoDocumentFilter[E]

  def projectionPaths: Opt[Set[String]]
  def decodeFrom(doc: BsonDocument): T
  def showRecordId: Boolean

  def withRecordId: MongoProjection[E, WithRecordId[T]] =
    MongoProjection.ShowRecordId(this)

  final def toProjectionBson: BsonDocument = projectionPaths.fold(Bson.document()) { paths =>
    val result = Bson.document(paths.iterator.map(_ -> Bson.int(1)))
    if (!result.containsKey(mongoId.Id)) {
      // partial projection didn't specify that it wants _id - exclude it explicitly
      result.put(mongoId.Id, Bson.int(0))
    }
    result
  }

  final def map[T0](fun: T => T0): MongoProjection[E, T0] =
    MongoProjection.Mapped(this, fun)

  final def map2[T0, T1](other: MongoProjection[E, T0])(fun: (T, T0) => T1): MongoProjection[E, T1] =
    MongoProjection.Composed(this, other, fun)

  final def zip[T0](other: MongoProjection[E, T0]): MongoProjection[E, (T, T0)] =
    map2(other)((_, _))
}

object MongoProjection extends ProjectionZippers {
  final class Empty[E] extends MongoProjection[E, Unit] {
    def impliedFilter: MongoDocumentFilter[E] = MongoDocumentFilter.empty
    def projectionPaths: Opt[Set[String]] = Opt(Set.empty)
    def showRecordId: Boolean = false
    def decodeFrom(doc: BsonDocument): Unit = ()
  }

  final case class Mapped[E, T, T0](prev: MongoProjection[E, T], fun: T => T0) extends MongoProjection[E, T0] {
    def impliedFilter: MongoDocumentFilter[E] = prev.impliedFilter
    def projectionPaths: Opt[Set[String]] = prev.projectionPaths
    def showRecordId: Boolean = prev.showRecordId
    def decodeFrom(doc: BsonDocument): T0 = fun(prev.decodeFrom(doc))
  }

  final case class Composed[E, T, T0, T1](
    first: MongoProjection[E, T], second: MongoProjection[E, T0], fun: (T, T0) => T1
  ) extends MongoProjection[E, T1] {
    def impliedFilter: MongoDocumentFilter[E] = first.impliedFilter && second.impliedFilter

    def projectionPaths: Opt[Set[String]] =
      for {
        firstPaths <- first.projectionPaths
        secondPaths <- second.projectionPaths
      } yield firstPaths ++ secondPaths

    def showRecordId: Boolean = first.showRecordId || second.showRecordId

    def decodeFrom(doc: BsonDocument): T1 =
      fun(first.decodeFrom(doc), second.decodeFrom(doc))
  }

  final val RecordId = "$recordId"

  final case class ShowRecordId[E, T](projection: MongoProjection[E, T]) extends MongoProjection[E, WithRecordId[T]] {
    def impliedFilter: MongoDocumentFilter[E] = projection.impliedFilter
    def projectionPaths: Opt[Set[String]] = projection.projectionPaths

    def decodeFrom(doc: BsonDocument): WithRecordId[T] =
      WithRecordId(projection.decodeFrom(doc), doc.getInt64(RecordId).getValue)

    def showRecordId: Boolean = true
  }
}

case class WithRecordId[T](data: T, recordId: Long)