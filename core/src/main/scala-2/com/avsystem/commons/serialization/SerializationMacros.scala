package com.avsystem.commons
package serialization

import com.avsystem.commons.derivation.AllowImplicitMacro

trait GenCodecMacros {
  def materialize[T]: GenCodec[T] = macro macros.serialization.GenCodecMacros.materialize[T]
  def fromApplyUnapplyProvider[T](applyUnapplyProvider: Any): GenCodec[T] =
    macro macros.serialization.GenCodecMacros.fromApplyUnapplyProvider[T]
  def applyUnapplyCodec[T]: ApplyUnapplyCodec[T] = macro macros.serialization.GenCodecMacros.applyUnapplyCodec[T]
  def forSealedEnum[T]: GenCodec[T] = macro macros.serialization.GenCodecMacros.forSealedEnum[T]
}

trait RecursiveAutoCodecs { this: GenCodec.type =>
  def materializeRecursively[T]: GenCodec[T] = macro macros.serialization.GenCodecMacros.materializeRecursively[T]
  implicit def materializeImplicitly[T](implicit allow: AllowImplicitMacro[GenCodec[T]]): GenCodec[T] =
    macro macros.serialization.GenCodecMacros.materializeImplicitly[T]
}

trait WhenAbsentMacros {
  def value[T]: T = macro com.avsystem.commons.macros.misc.WhiteMiscMacros.whenAbsentValue
}

trait GenObjectCodecMacros {
  def materialize[T]: GenObjectCodec[T] = macro macros.serialization.GenObjectCodecMacros.materialize[T]
  def fromApplyUnapplyProvider[T](applyUnapplyProvider: Any): GenObjectCodec[T] =
    macro macros.serialization.GenObjectCodecMacros.fromApplyUnapplyProvider[T]
}

trait ApplyUnapplyCodecMacros {
  def materialize[T]: ApplyUnapplyCodec[T] = macro macros.serialization.ApplyUnapplyCodecMacros.materialize[T]
}

trait GenKeyCodecMacros {
  def forSealedEnum[T]: GenKeyCodec[T] = macro macros.serialization.GenKeyCodecMacros.forSealedEnum[T]
  def forTransparentWrapper[T]: GenKeyCodec[T] = macro macros.serialization.GenKeyCodecMacros.forTransparentWrapper[T]
}

trait RawRefCreatorMacros[S] {
  def ref[T](fun: S => T): RawRef = macro macros.serialization.GenRefMacros.rawRef
}

trait GenRefCreatorMacros[S] {
  def ref[T](fun: S => T): GenRef[S, T] = macro macros.serialization.GenRefMacros.genRef
}

trait GenRefImplicitsMacros {
  implicit def fun2GenRef[S, T](fun: S => T): GenRef[S, T] = macro macros.serialization.GenRefMacros.genRef
}
