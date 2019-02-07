package com.avsystem.commons
package meta

import com.avsystem.commons.macros.meta.AdtMetadataMacros

trait AdtMetadataCompanion[M[_]] extends MetadataCompanion[M] {
  def materialize[T]: M[T] = macro AdtMetadataMacros.materialize[T]
}
