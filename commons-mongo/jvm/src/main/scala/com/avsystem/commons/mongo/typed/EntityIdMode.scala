package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.mongo.{BsonGenCodecs, mongoId}
import com.avsystem.commons.serialization.{GenCodec, TransparentWrapping}
import org.bson.types.ObjectId

import scala.annotation.implicitNotFound

/**
  * Typeclass that distinguishes between manual-ID Mongo entities and automatic-ID Mongo entities.
  * It is responsible for providing a [[MongoPropertyRef]] for ID of an entity.
  */
@implicitNotFound("Entity ${E} is invalid or has invalid ID type. " +
  "Note: entities must extend either `MongoEntity` or `AutoIdMongoEntity`. When extending `AutoIdMongoEntity` " +
  "the ID type must be raw `ObjectId` or a transparent wrapper over `ObjectId` (see `ObjectIdWrapperCompanion`).")
sealed trait EntityIdMode[E, ID] {
  def idRef(format: MongoAdtFormat[E]): MongoPropertyRef[E, ID] = this match {
    case EntityIdMode.Explicit() =>
      format.fieldRefFor(MongoRef.RootRef(format), MongoEntity.Id)
    case EntityIdMode.Auto(idWrapping) =>
      val idCodec = GenCodec.fromTransparentWrapping(idWrapping, BsonGenCodecs.objectIdCodec)
      MongoRef.FieldRef(MongoRef.RootRef(format), mongoId.Id, MongoFormat.Opaque(idCodec), Opt.Empty)
  }
}
object EntityIdMode {
  case class Explicit[E, ID]() extends EntityIdMode[E, ID]
  case class Auto[E, ID](idWrapping: TransparentWrapping[ObjectId, ID]) extends EntityIdMode[E, ID]

  implicit def explicitIdMode[E <: MongoEntity[ID], ID]: EntityIdMode[E, ID] = Explicit()

  implicit def autoIdMode[E <: AutoIdMongoEntity[ID], ID](
    implicit idWrapping: TransparentWrapping[ObjectId, ID]
  ): EntityIdMode[E, ID] = Auto(idWrapping)
}
