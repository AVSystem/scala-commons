package com.avsystem.commons
package serialization

import com.avsystem.commons.meta.MacroInstances
import com.avsystem.commons.misc.ValueOf

/**
  * Convenience abstract class for companion objects of types that have a [[GenCodec]].
  */
abstract class HasGenCodec[T](implicit macroCodec: MacroInstances[Unit, () => GenCodec[T]]) {
  implicit val codec: GenCodec[T] = macroCodec((), this).apply()
}

abstract class HasApplyUnapplyCodec[T](implicit macroCodec: MacroInstances[Unit, () => ApplyUnapplyCodec[T]]) {
  implicit val codec: ApplyUnapplyCodec[T] = macroCodec((), this).apply()
}

abstract class HasGenObjectCodec[T](implicit macroCodec: MacroInstances[Unit, () => GenObjectCodec[T]]) {
  implicit val codec: GenObjectCodec[T] = macroCodec((), this).apply()
}

abstract class HasGenCodecWithDeps[D, T](implicit deps: ValueOf[D], macroCodec: MacroInstances[D, () => GenCodec[T]]) {
  implicit val codec: GenCodec[T] = macroCodec(deps.value, this).apply()
}

abstract class HasApplyUnapplyCodecWithDeps[D, T](implicit deps: ValueOf[D], macroCodec: MacroInstances[D, () => ApplyUnapplyCodec[T]]) {
  implicit val codec: ApplyUnapplyCodec[T] = macroCodec(deps.value, this).apply()
}

abstract class HasGenObjectCodecWithDeps[D, T](implicit deps: ValueOf[D], macroCodec: MacroInstances[D, () => GenObjectCodec[T]]) {
  implicit val codec: GenObjectCodec[T] = macroCodec(deps.value, this).apply()
}
