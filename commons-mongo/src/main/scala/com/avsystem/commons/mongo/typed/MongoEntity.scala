package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.mongo.mongoId

/**
  * Type-member version of [[MongoEntity]]. Used in type bounds where wildcards would wreak havoc
  * (if `<: MongoEntity[_]` was used instead of `<: BaseMongoEntity`).
  */
sealed trait BaseMongoEntity {
  type IDType

  /**
    * ID of the entity. Maps to `_id` field in MongoDB documents.
    */
  @mongoId def id: IDType
}

/**
  * Base trait for all MongoDB entities.
  * A MongoDB entity must be a case class or a sealed hierarchy with `@flatten` annotation.
  * Its companion must extend [[MongoEntityCompanion]].
  *
  * @tparam ID type of `_id` field of the entity
  */
trait MongoEntity[ID] extends BaseMongoEntity {
  type IDType = ID
}
object MongoEntity {
  final val Id = "id"
}
