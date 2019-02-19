package com.avsystem.commons
package serialization

import com.avsystem.commons.meta.MacroInstances
import com.avsystem.commons.meta.MacroInstances.materializeWith
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

trait PolyCodec[C[_]] {
  def codec[T: GenCodec]: GenCodec[C[T]]
}

abstract class HasPolyGenCodec[C[_]](implicit macroCodec: MacroInstances[Unit, PolyCodec[C]]) {
  implicit def codec[T: GenCodec]: GenCodec[C[T]] = macroCodec((), this).codec
}

abstract class HasPolyGenCodecWithDeps[D, C[_]](implicit deps: ValueOf[D], macroCodec: MacroInstances[D, PolyCodec[C]]) {
  implicit def codec[T: GenCodec]: GenCodec[C[T]] = macroCodec(deps.value, this).codec
}

trait PolyObjectCodec[C[_]] {
  def codec[T: GenCodec]: GenObjectCodec[C[T]]
}

abstract class HasPolyGenObjectCodec[C[_]](implicit macroCodec: MacroInstances[Unit, PolyObjectCodec[C]]) {
  implicit def codec[T: GenCodec]: GenObjectCodec[C[T]] = macroCodec((), this).codec
}

abstract class HasPolyGenObjectCodecWithDeps[D, C[_]](implicit deps: ValueOf[D], macroCodec: MacroInstances[D, PolyObjectCodec[C]]) {
  implicit def codec[T: GenCodec]: GenObjectCodec[C[T]] = macroCodec(deps.value, this).codec
}

trait RecursiveCodec[T] {
  @materializeWith(GenCodec, "materializeRecursively")
  def codec: GenCodec[T]
}

abstract class HasRecursiveGenCodec[T](implicit instances: MacroInstances[Unit, RecursiveCodec[T]]) {
  implicit lazy val codec: GenCodec[T] = instances((), this).codec
}

trait CodecWithKeyCodec[T] {
  def codec: GenCodec[T]
  @materializeWith(GenKeyCodec, "forTransparentWrapper")
  def keyCodec: GenKeyCodec[T]
}

/**
  * Automatically injects both [[GenCodec]] and [[GenKeyCodec]]. The type must be a case class or case class like
  * type that wraps exactly one field for which [[GenKeyCodec]] exists.
  */
abstract class HasGenAndKeyCodec[T](implicit instances: MacroInstances[Unit, CodecWithKeyCodec[T]]) {
  implicit lazy val codec: GenCodec[T] = instances((), this).codec
  implicit lazy val keyCodec: GenKeyCodec[T] = instances((), this).keyCodec
}
