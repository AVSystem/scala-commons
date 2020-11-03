package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.annotation.{explicitGenerics, macroPrivate}
import com.avsystem.commons.macros.serialization.MongoMacros

trait DataRefDsl[E, T] {
  // just convenience type aliases
  type Ref[T0] = MongoRef[E, T0]
  type PropertyRef[T0] = MongoPropertyRef[E, T0]

  // ThisRef = MongoPropertyRef for MongoPropertyRef and MongoRef for all other types
  // This makes it possible to refine the result type of `as`, `compose`, `andThen` etc. in MongoPropertyRef
  // TODO: can we redesign this hierarchy to get rid of this abstraction and simplify things?
  type ThisRef[E0, T0] <: MongoRef[E0, T0]
  def SelfRef: ThisRef[E, T]

  // called by .ref macro to ensure that the source type is not opaque and inner references are possible
  @macroPrivate def asAdtRef(implicit ev: IsMongoAdtOrSubtype[T]): ThisRef[E, T] = SelfRef

  // this macro effectively calls `asAdtRef.fieldRefFor` while doing some additional static checks
  def ref[T0](fun: T => T0): MongoPropertyRef[E, T0] = macro MongoMacros.refImpl

  // this macro effectively calls `asAdtRef.subtypeRefFor` while doing some additional static checks
  @explicitGenerics
  def as[C <: T]: ThisRef[E, C] = macro MongoMacros.asSubtype[C]

  // this macro effectively calls `asAdtRef.subtypeFilterFor` while doing some additional static checks
  @explicitGenerics
  def is[C <: T]: MongoDocumentFilter[E] = macro MongoMacros.isSubtype[C]

  // this macro effectively calls `asAdtRef.subtypeFilterFor` while doing some additional static checks
  @explicitGenerics
  def isNot[C <: T]: MongoDocumentFilter[E] = macro MongoMacros.isNotSubtype[C]
}

trait DataTypeDsl[T] extends DataRefDsl[T, T] {
  type ThisRef[E0, T0] = MongoRef[E0, T0]
}
