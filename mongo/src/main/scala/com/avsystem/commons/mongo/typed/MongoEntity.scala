package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.mongo.mongoId

/** Type-member version of [[MongoEntity]]. Used in type bounds where wildcards would wreak havoc (if
  * `<: MongoEntity[_]` was used instead of `<: BaseMongoEntity`).
  */
sealed trait BaseMongoEntity {
  type IDType
}

/** Base trait for all MongoDB entities. A MongoDB entity must be a case class or a sealed hierarchy with `@flatten`
  * annotation. Its companion must extend [[MongoEntityCompanion]].
  *
  * @tparam ID
  *   type of `_id` field of the entity
  */
trait MongoEntity[ID] extends BaseMongoEntity {
  type IDType = ID

  /** ID of the entity. Maps to `_id` field in MongoDB documents.
    */
  @mongoId def id: IDType
}
object MongoEntity {
  final val Id = "id"
}

/** Base trait for MongoDB entities whose ID is automatically managed by MongoDB. This means that the entity class in
  * Scala does not (and MUST NOT) contain `_id` as one of its fields (as is required for regular [[MongoEntity]]).
  *
  * Auto-ID entities are useful when there is no obvious choice for an unique and immutable `_id` and usually there is
  * no need to access it directly (e.g because queries are based on other fields).
  *
  * @tparam ID
  *   type of the (auto-generated) ID - it MUST be either raw [[org.bson.types.ObjectId]] or a _transparent wrapper_
  *   over `ObjectId`, e.g. via [[ObjectIdWrapperCompanion]].
  */
trait AutoIdMongoEntity[ID] extends BaseMongoEntity {
  type IDType = ID
}
