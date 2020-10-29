package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.annotation.macroPrivate
import com.avsystem.commons.macros.serialization.MongoMacros

trait DataTypeDsl[E, T] {
  type ThisDataRef[T0 <: T] <: MongoRef[E, T0]

  @macroPrivate def thisDataRef: ThisDataRef[T]

  // this macro effectively calls `fieldRefFor` while doing some additional static checks
  def ref[T0](fun: T => T0): MongoPropertyRef[E, T0] = macro MongoMacros.refImpl

  // this macro effectively calls `subtypeRefFor` while doing some additional static checks
  def as[C <: T]: ThisDataRef[C] = macro MongoMacros.asSubtype[C]

  // this macro effectively calls `subtypeConditionFor` while doing some additional static checks
  def is[C <: T]: MongoDocumentFilter[E] = macro MongoMacros.isSubtype[C]
}
