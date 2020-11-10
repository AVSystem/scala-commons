package com.avsystem.commons
package mongo.typed

import com.mongodb.client.model._

/**
  * Represents a single MongoDB write operation in a
  * [[https://docs.mongodb.com/manual/core/bulk-write-operations/ bulk write operation]].
  *
  * @tparam E type of the entity
  */
sealed trait MongoWrite[E] {

  import MongoWrite._

  def toWriteModel: WriteModel[E] = this match {
    case InsertOne(value) =>
      new InsertOneModel(value)
    case UpdateOne(filter, update, setupOptions) =>
      new UpdateOneModel(filter.toBson, update.toBson, setupOptions(new UpdateOptions))
    case UpdateMany(filter, update, setupOptions) =>
      new UpdateManyModel(filter.toBson, update.toBson, setupOptions(new UpdateOptions))
    case ReplaceOne(filter, replacement, setupOptions) =>
      new ReplaceOneModel(filter.toBson, replacement, setupOptions(new ReplaceOptions))
    case DeleteOne(filter, setupOptions) =>
      new DeleteOneModel(filter.toBson, setupOptions(new DeleteOptions))
    case DeleteMany(filter, setupOptions) =>
      new DeleteManyModel(filter.toBson, setupOptions(new DeleteOptions))
  }
}
object MongoWrite {
  final case class InsertOne[E](
    value: E
  ) extends MongoWrite[E]

  final case class UpdateOne[E](
    filter: MongoDocumentFilter[E],
    update: MongoDocumentUpdate[E],
    setupOptions: UpdateOptions => UpdateOptions = identity
  ) extends MongoWrite[E]

  final case class UpdateMany[E](
    filter: MongoDocumentFilter[E],
    update: MongoDocumentUpdate[E],
    setupOptions: UpdateOptions => UpdateOptions = identity
  ) extends MongoWrite[E]

  final case class ReplaceOne[E](
    filter: MongoDocumentFilter[E],
    replacement: E,
    setupOptions: ReplaceOptions => ReplaceOptions = identity
  ) extends MongoWrite[E]

  final case class DeleteOne[E](
    filter: MongoDocumentFilter[E],
    setupOptions: DeleteOptions => DeleteOptions = identity
  ) extends MongoWrite[E]

  final case class DeleteMany[E](
    filter: MongoDocumentFilter[E],
    setupOptions: DeleteOptions => DeleteOptions = identity
  ) extends MongoWrite[E]
}
