package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.annotation.{explicitGenerics, macroPrivate}
import com.avsystem.commons.macros.serialization.MongoMacros

trait DataRefDsl[E, T] {
  type ThisRef[T0 <: T] <: MongoRef[E, T0]

  protected def thisRef: ThisRef[T]

  @macroPrivate def asAdtRef(implicit ev: IsMongoAdtOrSubtype[T]): ThisRef[T] = thisRef

  // this macro effectively calls `asAdtRef.fieldRefFor` while doing some additional static checks
  def ref[T0](fun: T => T0): MongoPropertyRef[E, T0] = macro MongoMacros.refImpl

  // this macro effectively calls `asAdtRef.subtypeRefFor` while doing some additional static checks
  @explicitGenerics
  def as[C <: T]: ThisRef[C] = macro MongoMacros.asSubtype[C]

  // this macro effectively calls `asAdtRef.subtypeFilterFor` while doing some additional static checks
  @explicitGenerics
  def is[C <: T]: MongoDocumentFilter[E] = macro MongoMacros.isSubtype[C]

  // this macro effectively calls `asAdtRef.subtypeFilterFor` while doing some additional static checks
  @explicitGenerics
  def isNot[C <: T]: MongoDocumentFilter[E] = macro MongoMacros.isNotSubtype[C]
}

trait DataTypeDsl[T] extends DataRefDsl[T, T]
