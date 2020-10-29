package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.annotation.macroPrivate
import com.avsystem.commons.macros.serialization.MongoMacros

trait DataRefDsl[E, T] {
  type ThisRef[T0 <: T] <: MongoRef[E, T0]

  protected def thisRef: ThisRef[T]

  @macroPrivate def asAdtRef(implicit ev: IsMongoAdtOrSubtype[T]): ThisRef[T] = thisRef

  // this macro effectively calls `fieldRefFor` while doing some additional static checks
  def ref[T0](fun: T => T0): MongoPropertyRef[E, T0] = macro MongoMacros.refImpl

  // this macro effectively calls `subtypeRefFor` while doing some additional static checks
  def as[C <: T]: ThisRef[C] = macro MongoMacros.asSubtype[C]

  // this macro effectively calls `subtypeConditionFor` while doing some additional static checks
  def is[C <: T]: MongoDocumentFilter[E] = macro MongoMacros.isSubtype[C]
}

trait DataTypeDsl[T] extends DataRefDsl[T, T]
