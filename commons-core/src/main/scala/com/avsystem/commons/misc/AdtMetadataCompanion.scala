package com.avsystem.commons
package misc

import com.avsystem.commons.macros.misc.AdtMetadataMacros

trait AdtMetadataCompanion[M[_]] extends MetadataCompanion[M] {
  def materialize[T]: M[T] = macro AdtMetadataMacros.materialize[T]

  implicit def materializeMacroGenerated[T]: MacroGenerated[M[T]] = macro AdtMetadataMacros.materializeMacroGenerated[T]
}
