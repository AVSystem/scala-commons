package com.avsystem.commons
package mongo.model

import com.avsystem.commons.mongo.mongoId
import org.bson.BsonDocument

trait MongoProjection[E, T] {
  def impliedFilter: MongoDocumentFilter[E]

  def documentPaths: Opt[Set[String]]
  def decode(doc: BsonDocument): T
  def showRecordId: Boolean

  def withRecordId: MongoProjection[E, WithRecordId[T]] =
    MongoProjection.ShowRecordId(this)

  final def toProjectionBson: BsonDocument = documentPaths.fold(Bson.document()) { paths =>
    val result = Bson.document(paths.iterator.map(_ -> Bson.int(1)))
    if (!result.containsKey(mongoId.Id)) {
      // partial projection didn't specify that it wants _id - exclude it explicitly
      result.put(mongoId.Id, Bson.int(-1))
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

object MongoProjection {
  final class Empty[E] extends MongoProjection[E, Unit] {
    def impliedFilter: MongoDocumentFilter[E] = MongoDocumentFilter.empty
    def documentPaths: Opt[Set[String]] = Opt(Set.empty)
    def showRecordId: Boolean = false
    def decode(doc: BsonDocument): Unit = ()
  }

  final case class Mapped[E, T, T0](prev: MongoProjection[E, T], fun: T => T0) extends MongoProjection[E, T0] {
    def impliedFilter: MongoDocumentFilter[E] = prev.impliedFilter
    def documentPaths: Opt[Set[String]] = prev.documentPaths
    def showRecordId: Boolean = prev.showRecordId
    def decode(doc: BsonDocument): T0 = fun(prev.decode(doc))
  }

  final case class Composed[E, T, T0, T1](
    first: MongoProjection[E, T], second: MongoProjection[E, T0], fun: (T, T0) => T1
  ) extends MongoProjection[E, T1] {
    def impliedFilter: MongoDocumentFilter[E] = first.impliedFilter && second.impliedFilter

    def documentPaths: Opt[Set[String]] =
      for {
        firstPaths <- first.documentPaths
        secondPaths <- second.documentPaths
      } yield firstPaths ++ secondPaths

    def showRecordId: Boolean = first.showRecordId || second.showRecordId

    def decode(doc: BsonDocument): T1 =
      fun(first.decode(doc), second.decode(doc))
  }

  final val RecordId = "$recordId"

  final case class ShowRecordId[E, T](projection: MongoProjection[E, T]) extends MongoProjection[E, WithRecordId[T]] {
    def impliedFilter: MongoDocumentFilter[E] = projection.impliedFilter
    def documentPaths: Opt[Set[String]] = projection.documentPaths

    def decode(doc: BsonDocument): WithRecordId[T] =
      WithRecordId(projection.decode(doc), doc.getInt64(RecordId).getValue)

    def showRecordId: Boolean = true
  }
}

case class WithRecordId[T](data: T, recordId: Long)
