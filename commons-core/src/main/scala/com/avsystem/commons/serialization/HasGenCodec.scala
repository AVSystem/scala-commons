package com.avsystem.commons
package serialization

import com.avsystem.commons.meta.MacroInstances

/**
  * Convenience abstract class for companion objects of types that have a [[GenCodec]].
  */
abstract class HasGenCodec[T](implicit macroCodec: MacroInstances[Unit, () => GenCodec[T]]) {
  implicit val codec: GenCodec[T] = macroCodec((), this).apply()
}

abstract class HasApplyUnapplyCodec[T](implicit macroCodec: MacroInstances[Unit, () => ApplyUnapplyCodec[T]]) {
  implicit val codec: ApplyUnapplyCodec[T] = macroCodec((), this).apply()
}
