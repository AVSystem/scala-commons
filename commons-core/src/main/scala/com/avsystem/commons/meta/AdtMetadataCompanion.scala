package com.avsystem.commons
package meta

import com.avsystem.commons.macros.meta.AdtMetadataMacros

trait BoundedAdtMetadataCompanion[Hi, Lo <: Hi, M[_ >: Lo <: Hi]] extends BoundedMetadataCompanion[Hi, Lo, M] {
  def materialize[T >: Lo <: Hi]: M[T] = macro AdtMetadataMacros.materialize[T]

  def fromApplyUnapplyProvider[T >: Lo <: Hi](applyUnapplyProvider: Any): M[T] = macro AdtMetadataMacros.fromApplyUnapplyProvider[T]
}

trait AdtMetadataCompanion[M[_]] extends BoundedAdtMetadataCompanion[Any, Nothing, M]
