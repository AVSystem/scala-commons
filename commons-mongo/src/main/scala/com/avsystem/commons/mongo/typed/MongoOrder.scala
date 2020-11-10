package com.avsystem.commons
package mongo.typed

import org.bson.{BsonDocument, BsonValue}

/**
  * Represents a MongoDB order used to sort values. In most situations what you want is [[MongoDocumentOrder]].
  * The base `MongoOrder` type is used only in rare situations where `T` is not guaranteed to be a document, e.g.
  * in array `$$push` operator (see `UpdateOperatorsDsl.ForCollection.push`).
  *
  * @tparam T type of the value being sorted
  */
sealed trait MongoOrder[T] {
  def toBson: BsonValue
}
object MongoOrder {
  def empty[E]: MongoDocumentOrder[E] = MongoDocumentOrder.empty[E]

  def ascending[T]: MongoOrder[T] = Simple(true)
  def descending[T]: MongoOrder[T] = Simple(false)

  def simple[T](ascending: Boolean): MongoOrder[T] = Simple(ascending)

  def apply[E](refs: (MongoPropertyRef[E, _], Boolean)*): MongoDocumentOrder[E] =
    MongoDocumentOrder(refs: _*)

  final case class Simple[T](ascending: Boolean) extends MongoOrder[T] {
    def toBson: BsonValue = Bson.int(if (ascending) 1 else -1)
  }
}

/**
  * Represents a MongoDB sort order. Sort order is used to sort documents, usually in results of a query.
  *
  * Examples:
  * {{{
  *   case class MyEntity(id: String, int: Int, num: Double) extends MongoEntity[String]
  *   object MyEntity extends MongoEntityCompanion[MyEntity]
  *
  *   // {"_id": 1}
  *   MyEntity.ref(_.id).ascending
  *
  *   // {"int": 1, "num": -1}
  *   MyEntity.ref(_.int).ascending andThen MyEntity.ref(_.num).descending
  *
  *   // {"int": 1, "num": -1}
  *   MongoDocumentOrder(MyEntity.ref(_.int) -> true, MyEntity.ref(_.num) -> false)
  *
  *   // {"int": 1, "num": 1}
  *   MongoDocumentOrder.ascending(MyEntity.ref(_.int), MyEntity.ref(_.num))
  * }}}
  *
  * @tparam E type of the entity/document
  */
case class MongoDocumentOrder[E](refs: Vector[(MongoPropertyRef[E, _], Boolean)]) extends MongoOrder[E] {
  def andThen(other: MongoDocumentOrder[E]): MongoDocumentOrder[E] =
    MongoDocumentOrder(refs ++ other.refs)

  def andThenBy(ref: MongoPropertyRef[E, _], ascending: Boolean): MongoDocumentOrder[E] =
    MongoDocumentOrder(refs :+ (ref -> ascending))

  def andThenAscendingBy(ref: MongoPropertyRef[E, _]): MongoDocumentOrder[E] =
    andThenBy(ref, ascending = true)

  def andThenDescendingBy(ref: MongoPropertyRef[E, _]): MongoDocumentOrder[E] =
    andThenBy(ref, ascending = false)

  //TODO: lambda-macro versions of andThenBy

  def toBson: BsonDocument =
    Bson.document(refs.iterator.map { case (ref, asc) => (ref.filterPath, Bson.int(if (asc) 1 else -1)) })
}
object MongoDocumentOrder {
  def empty[E]: MongoDocumentOrder[E] = MongoDocumentOrder(Vector.empty)

  def apply[E](refs: (MongoPropertyRef[E, _], Boolean)*): MongoDocumentOrder[E] =
    MongoDocumentOrder(refs.toVector)

  def ascending[E](refs: MongoPropertyRef[E, _]*): MongoDocumentOrder[E] =
    MongoDocumentOrder(refs.iterator.map(r => r -> true).toVector)

  def descending[E](refs: MongoPropertyRef[E, _]*): MongoDocumentOrder[E] =
    MongoDocumentOrder(refs.iterator.map(r => r -> false).toVector)
}
