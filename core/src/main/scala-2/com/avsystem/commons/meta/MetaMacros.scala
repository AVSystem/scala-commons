package com.avsystem.commons
package meta

import com.avsystem.commons.macros.meta.AdtMetadataMacros
import com.avsystem.commons.macros.meta.MetadataMacros
import com.avsystem.commons.macros.misc.MiscMacros

trait InferMacros {
  def value[T]: T = macro com.avsystem.commons.macros.misc.WhiteMiscMacros.inferValue
}

trait AdtMetadataCompanionMacros[M[_]] {
  def materialize[T]: M[T] = macro AdtMetadataMacros.materialize[T]
  def fromApplyUnapplyProvider[T](applyUnapplyProvider: Any): M[T] = macro AdtMetadataMacros.fromApplyUnapplyProvider[T]
}

trait BoundedAdtMetadataCompanionMacros[Hi, Lo <: Hi, M[_ >: Lo <: Hi]] {
  def materialize[T >: Lo <: Hi]: M[T] = macro AdtMetadataMacros.materialize[T]
  def fromApplyUnapplyProvider[T >: Lo <: Hi](applyUnapplyProvider: Any): M[T] = macro AdtMetadataMacros.fromApplyUnapplyProvider[T]
}

trait MetadataCompanionMacros[M[_]] {
  def materialize[T]: M[T] = macro MetadataMacros.materialize[T]
  def fromApplyUnapplyProvider[T](applyUnapplyProvider: Any): M[T] = macro MetadataMacros.fromApplyUnapplyProvider[T]
}

trait BoundedMetadataCompanionMacros[Hi, Lo <: Hi, M[_ >: Lo <: Hi]] {
  def materialize[T >: Lo <: Hi]: M[T] = macro MetadataMacros.materialize[T]
  def fromApplyUnapplyProvider[T >: Lo <: Hi](applyUnapplyProvider: Any): M[T] = macro MetadataMacros.fromApplyUnapplyProvider[T]
}

trait MetadataCompanionLazyMacros[M[_], Lazy[_]] {
  implicit def lazyMetadata[Real](implicit metadata: M[Real]): Lazy[Real] = macro MiscMacros.lazyMetadata
}

trait BoundedMetadataCompanionLazyMacros[Hi, Lo <: Hi, M[_ >: Lo <: Hi], Lazy[_ >: Lo <: Hi]] {
  implicit def lazyMetadata[Real >: Lo <: Hi](implicit metadata: M[Real]): Lazy[Real] = macro MiscMacros.lazyMetadata
}

trait MacroInstancesMacros {
  implicit def materialize[Implicits, Instances]: MacroInstances[Implicits, Instances] =
    macro com.avsystem.commons.macros.misc.MiscMacros.macroInstances
}
