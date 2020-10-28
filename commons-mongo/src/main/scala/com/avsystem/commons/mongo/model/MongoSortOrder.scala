package com.avsystem.commons
package mongo.model

import org.bson.BsonDocument

case class MongoSortOrder[E](refs: Vector[(MongoPropertyRef[E, _], Boolean)]) {
  def andThen(other: MongoSortOrder[E]): MongoSortOrder[E] =
    MongoSortOrder(refs ++ other.refs)

  def andThenBy(ref: MongoPropertyRef[E, _], ascending: Boolean): MongoSortOrder[E] =
    MongoSortOrder(refs :+ (ref -> ascending))

  def andThenAscendingBy(ref: MongoPropertyRef[E, _]): MongoSortOrder[E] =
    andThenBy(ref, ascending = true)

  def andThenDescendingBy(ref: MongoPropertyRef[E, _]): MongoSortOrder[E] =
    andThenBy(ref, ascending = false)

  //TODO: lambda-macro versions of andThenBy

  def toBson: BsonDocument =
    Bson.document(refs.iterator.map { case (ref, asc) => (ref.propertyPath, Bson.int(if (asc) 1 else -1)) })
}
object MongoSortOrder {
  def empty[E]: MongoSortOrder[E] = MongoSortOrder(Vector.empty)

  def apply[E](refs: (MongoPropertyRef[E, _], Boolean)*): MongoSortOrder[E] =
    MongoSortOrder(refs.toVector)
}
