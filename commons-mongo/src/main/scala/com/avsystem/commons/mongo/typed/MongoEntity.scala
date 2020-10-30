package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.annotation.explicitGenerics
import com.avsystem.commons.meta.MacroInstances
import com.avsystem.commons.mongo.{BsonGenCodecs, mongoId}
import com.avsystem.commons.serialization.GenObjectCodec

import scala.annotation.{compileTimeOnly, implicitNotFound}

sealed trait BaseMongoEntity {
  type IDType
  @mongoId def id: IDType
}

trait MongoEntity[ID] extends BaseMongoEntity {
  type IDType = ID
}
object MongoEntity {
  final val Id = "id"
}

trait MongoAdtInstances[T] {
  def codec: GenObjectCodec[T]
  def format: MongoAdtFormat[T]
}

/**
  * Provides additional static validation for `as`, `is` and `ref` macros from [[DataTypeDsl]].
  * Catches mistakes when someone forgets to use [[MongoDataCompanion]] or [[MongoEntityCompanion]] for its
  * case class or sealed hierarchy.
  */
@implicitNotFound("${T} is an opaque data type - does it have a companion that extends MongoDataCompanion?")
sealed trait IsMongoAdtOrSubtype[T]

sealed trait RefMacroDslExtensions[E] {
  @explicitGenerics
  def as[T <: E]: T
}

abstract class AbstractMongoDataCompanion[Implicits, E](implicits: Implicits)(
  implicit instances: MacroInstances[Implicits, MongoAdtInstances[E]]
) extends DataTypeDsl[E] {
  implicit val codec: GenObjectCodec[E] = instances(implicits, this).codec
  implicit val format: MongoAdtFormat[E] = instances(implicits, this).format

  implicit def isMongoAdtOrSubtype[C <: E]: IsMongoAdtOrSubtype[C] = null

  @compileTimeOnly("the .as[Subtype] construct can only be used inside lambda passed to .ref(...) macro")
  implicit def refMacroDslExtensions(e: E): RefMacroDslExtensions[E] = sys.error("stub")

  type PropertyRef[T] = MongoPropertyRef[E, T]
  type TypeRef[C <: E] = MongoDataRef[E, C]

  final val SelfRef: TypeRef[E] = MongoRef.SelfRef(format)

  type ThisRef[C <: E] = MongoDataRef[E, C]

  protected def thisRef: ThisRef[E] = SelfRef
}

abstract class AbstractMongoEntityCompanion[Implicits, E <: BaseMongoEntity](implicits: Implicits)(
  implicit instances: MacroInstances[Implicits, MongoAdtInstances[E]]
) extends AbstractMongoDataCompanion[Implicits, E](implicits) {

  type ID = E#IDType

  final val IdRef: PropertyRef[ID] = format.fieldRefFor(SelfRef, MongoEntity.Id)
}

abstract class MongoDataCompanion[E](
  implicit instances: MacroInstances[BsonGenCodecs.type, MongoAdtInstances[E]]
) extends AbstractMongoDataCompanion[BsonGenCodecs.type, E](BsonGenCodecs)

abstract class MongoEntityCompanion[E <: BaseMongoEntity](
  implicit instances: MacroInstances[BsonGenCodecs.type, MongoAdtInstances[E]]
) extends AbstractMongoEntityCompanion[BsonGenCodecs.type, E](BsonGenCodecs)
