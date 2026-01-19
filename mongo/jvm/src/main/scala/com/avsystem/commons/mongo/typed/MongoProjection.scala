package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.mongo.mongoId
import org.bson.BsonDocument

import scala.annotation.tailrec

/** Determines document fields to include in results of a query.
  *
  * Projection also defines how the (possibly partial) query result is deserialized into a final type `T`. This may
  * include some application-level transformations.
  *
  * Some projections are associated with an *implied filter* which may limit the results only to some documents. For
  * example, a projection may narrow the results of a query only to one particular case class in a sealed hierarchy (see
  * [[DataTypeDsl.as]]).
  *
  * The simplest, trivial projection is a [[DataRefDsl.SelfRef]] (e.g. `MyEntity.SelfRef`) which returns the whole
  * entity. If the entity is a sealed hierarchy then the query can be limited and "cast" to a subtype of that sealed
  * trait/class, e.g. `MyUnionEntity.as[SomeCaseClass]`
  *
  * A projection may also be a [[MongoPropertyRef]] - reference to a single path inside a document.
  *
  * Finally, you can create multi-field projections using one of the [[MongoProjection]]`.zip` overloads in order to
  * compose multiple projections into a single one that returns a tuple.
  *
  * Every projection may also be mapped with an application-level function that transforms the returned value in an
  * arbitrary way.
  *
  * @tparam E
  *   type of the entity
  * @tparam T
  *   type of the value extracted by this projection
  * @see
  *   [[https://docs.mongodb.com/manual/tutorial/project-fields-from-query-results/]]
  */
trait MongoProjection[E, T] {
  def projectionRefs: Set[MongoRef[E, _]]
  def decodeFrom(doc: BsonDocument): T
  def showRecordId: Boolean

  /** Instructs the server to return the _record ID_ with the result.
    * @see
    *   [[https://docs.mongodb.com/manual/reference/method/cursor.showRecordId/#cursor.showRecordId]]
    */
  def withRecordId: MongoProjection[E, WithRecordId[T]] =
    MongoProjection.ShowRecordId(this)

  /** Adds an application-level mapping function to this projection. The function is invoked in application and does not
    * affect the MongoDB projection document sent with the query.
    */
  final def map[T0](fun: T => T0): MongoProjection[E, T0] =
    new MongoProjection.Mapped(this, fun)

  /** Combines two projections into one by applying a two-argument function to the results of two original projections.
    * The function runs fully in application and does not affect the MongoDB projection document sent with the query
    * (which is a sum of the fields specified by the two composed projections).
    */
  final def map2[T0, T1](other: MongoProjection[E, T0])(fun: (T, T0) => T1): MongoProjection[E, T1] =
    new MongoProjection.Composed(this, other, fun)

  final def zip[T0](other: MongoProjection[E, T0]): MongoProjection[E, (T, T0)] =
    map2(other)((_, _))

  /** Applies a "prefix" to this projection. This effectively adds a prefix path to all the paths referenced by this
    * projection. This may be used to turn a projection for an inner document into a projection for an outer document
    * that contains it.
    */
  def on[E0](ref: MongoRef[E0, E]): MongoProjection[E0, T]

  final def toProjectionBson: BsonDocument = {
    val doc = new BsonDocument
    @tailrec def loop(it: Iterator[MongoRef[E, _]]): Unit =
      if (it.hasNext) it.next() match {
        case propRef: MongoPropertyRef[E, _] =>
          doc.put(propRef.projectionPath, Bson.int(1))
          loop(it)
        case _: MongoToplevelRef[E, _] =>
          doc.clear()
      }
    loop(projectionRefs.iterator)
    if (!doc.isEmpty && !doc.containsKey(mongoId.Id)) {
      doc.put(mongoId.Id, Bson.int(0))
    }
    doc
  }
}

object MongoProjection extends ProjectionZippers {
  final class Mapped[E, T, T0](prev: MongoProjection[E, T], fun: T => T0) extends MongoProjection[E, T0] {
    def projectionRefs: Set[MongoRef[E, _]] = prev.projectionRefs
    def showRecordId: Boolean = prev.showRecordId
    def decodeFrom(doc: BsonDocument): T0 = fun(prev.decodeFrom(doc))
    def on[E0](ref: MongoRef[E0, E]): MongoProjection[E0, T0] = new Mapped(prev.on(ref), fun)
  }

  final class Composed[E, T, T0, T1](
    first: MongoProjection[E, T],
    second: MongoProjection[E, T0],
    fun: (T, T0) => T1,
  ) extends MongoProjection[E, T1] {
    def projectionRefs: Set[MongoRef[E, _]] =
      first.projectionRefs ++ second.projectionRefs

    def showRecordId: Boolean = first.showRecordId || second.showRecordId

    def decodeFrom(doc: BsonDocument): T1 =
      fun(first.decodeFrom(doc), second.decodeFrom(doc))

    def on[E0](ref: MongoRef[E0, E]): MongoProjection[E0, T1] =
      new Composed(first.on(ref), second.on(ref), fun)
  }

  final val RecordId = "$recordId"

  final case class ShowRecordId[E, T](projection: MongoProjection[E, T]) extends MongoProjection[E, WithRecordId[T]] {
    def projectionRefs: Set[MongoRef[E, _]] = projection.projectionRefs

    def decodeFrom(doc: BsonDocument): WithRecordId[T] =
      WithRecordId(projection.decodeFrom(doc), doc.getInt64(RecordId).getValue)

    def showRecordId: Boolean = true

    def on[E0](ref: MongoRef[E0, E]): MongoProjection[E0, WithRecordId[T]] =
      ShowRecordId(projection.on(ref))
  }
}

case class WithRecordId[T](data: T, recordId: Long)
