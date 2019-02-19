package com.avsystem.commons
package meta

import com.avsystem.commons.macros.meta.AdtMetadataMacros
import com.avsystem.commons.misc.MacroGenerated

trait AdtMetadataCompanion[M[_]] extends MetadataCompanion[M] {
  def materialize[T]: M[T] = macro AdtMetadataMacros.materialize[T]

  def fromApplyUnapplyProvider[T](applyUnapplyProvider: Any): M[T] = macro AdtMetadataMacros.fromApplyUnapplyProvider[T]
}
