package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.annotation.explicitGenerics
import com.avsystem.commons.meta.MacroInstances
import com.avsystem.commons.mongo.BsonGenCodecs
import com.avsystem.commons.serialization.{GenCodec, GenObjectCodec}

import scala.annotation.compileTimeOnly

trait MongoPolyAdtInstances[D[_]] {
  def codec[T: GenCodec]: GenObjectCodec[D[T]]

  /**
    * We need to accept an implicit `GenObjectCodec[D[T]]` because materialization of
    * [[MongoAdtFormat]] requires it ([[MongoAdtFormat.codec]]). In practice, it can be derived
    * from the `MongoFormat[T]` that is already accepted by this method but we don't want to do this in this
    * trait to avoid problems with priorities of implicits. Because of that, this implicit is actually constructed
    * by [[AbstractMongoPolyDataCompanion.format]].
    */
  def format[T: MongoFormat](implicit codec: GenObjectCodec[D[T]]): MongoAdtFormat[D[T]]
}

abstract class AbstractMongoPolyDataCompanion[Implicits, D[_]](implicits: Implicits)(
  implicit instances: MacroInstances[Implicits, MongoPolyAdtInstances[D]],
) {
  implicit def codec[T: GenCodec]: GenObjectCodec[D[T]] = instances(implicits, this).codec[T]

  implicit def format[T: MongoFormat]: MongoAdtFormat[D[T]] = {
    implicit def tCodec: GenCodec[T] = MongoFormat[T].codec
    instances(implicits, this).format[T]
  }

  implicit def isMongoAdtOrSubtype[C <: D[_]]: IsMongoAdtOrSubtype[C] = null

  implicit class macroDslExtensions[T](value: D[T]) {
    @explicitGenerics
    @compileTimeOnly("the .as[Subtype] construct can only be used inside lambda passed to .ref(...) macro")
    def as[C <: D[T]]: C = sys.error("stub")
  }

  @explicitGenerics
  def dsl[T: MongoFormat]: DataTypeDsl[D[T]] = new DataTypeDsl[D[T]] {
    def SelfRef: MongoRef[D[T], D[T]] = MongoRef.RootRef(format[T])
  }
}

/**
  * Like [[MongoDataCompanion]] buf for generic types (with exactly one unbounded type parameter).
  *
  * @example
  * {{{
  *   case class Point[+T](x: T, y: T)
  *   object Point extends MongoPolyDataCompanion[Point] {
  *     def XRef[T: MongoFormat]: MongoPropertyRef[Point[T], T] =
  *       Point.dsl[T].ref(_.x)
  *   }
  * }}}
  */
abstract class MongoPolyDataCompanion[D[_]](
  implicit instances: MacroInstances[BsonGenCodecs.type, MongoPolyAdtInstances[D]],
) extends AbstractMongoPolyDataCompanion[BsonGenCodecs.type, D](BsonGenCodecs)
