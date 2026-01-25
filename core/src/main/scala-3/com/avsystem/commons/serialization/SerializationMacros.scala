package com.avsystem.commons
package serialization

import com.avsystem.commons.derivation.AllowImplicitMacro
import scala.quoted.*

trait GenCodecMacros {
  inline def materialize[T]: GenCodec[T] = ${ SerializationMacros.materializeImpl[T, GenCodec] }
  inline def fromApplyUnapplyProvider[T](inline applyUnapplyProvider: Any): GenCodec[T] =
    ${ SerializationMacros.fromApplyUnapplyProviderImpl[T, GenCodec]('applyUnapplyProvider) }
  inline def applyUnapplyCodec[T]: ApplyUnapplyCodec[T] = ${ SerializationMacros.applyUnapplyCodecImpl[T] }
  def forSealedEnum[T]: GenCodec[T] = ???
}

trait RecursiveAutoCodecs { this: GenCodec.type =>
  inline def materializeRecursively[T]: GenCodec[T] = ${ SerializationMacros.materializeRecursivelyImpl[T] }
  inline implicit def materializeImplicitly[T](implicit allow: AllowImplicitMacro[GenCodec[T]]): GenCodec[T] =
    ${ SerializationMacros.materializeImplicitlyImpl('allow) }
}

trait WhenAbsentMacros {
  def value[T]: T = ???
}

trait GenObjectCodecMacros {
  inline def materialize[T]: GenObjectCodec[T] = ${ SerializationMacros.materializeImpl[T, GenObjectCodec] }
  inline def fromApplyUnapplyProvider[T](inline applyUnapplyProvider: Any): GenObjectCodec[T] =
    ${ SerializationMacros.fromApplyUnapplyProviderImpl[T, GenObjectCodec]('applyUnapplyProvider) }
}

trait ApplyUnapplyCodecMacros {
  inline def materialize[T]: ApplyUnapplyCodec[T] = ${ SerializationMacros.materializeImpl[T, ApplyUnapplyCodec] }
}

trait GenKeyCodecMacros {
  def forSealedEnum[T]: GenKeyCodec[T] = ???
  def forTransparentWrapper[T]: GenKeyCodec[T] = ???
}

trait RawRefCreatorMacros[S] {
  inline def ref[T](fun: S => T): RawRef = ${ SerializationMacros.refImpl[S, T]('fun) }
}

trait GenRefCreatorMacros[S] {
  inline def ref[T](fun: S => T): GenRef[S, T] = ${ SerializationMacros.refImpl[S, T]('fun) }
}

trait GenRefImplicitsMacros {
  inline implicit def fun2GenRef[S, T](fun: S => T): GenRef[S, T] = ${ SerializationMacros.fun2GenRefImpl[S, T]('fun) }
}

object SerializationMacros {
  def materializeImpl[T: Type, R[_]: Type](using Quotes): Expr[R[T]] = '{ ??? }.asInstanceOf[Expr[R[T]]]
  def fromApplyUnapplyProviderImpl[T: Type, R[_]: Type](applyUnapplyProvider: Expr[Any])(using Quotes): Expr[R[T]] = '{ ??? }.asInstanceOf[Expr[R[T]]]
  def applyUnapplyCodecImpl[T: Type](using Quotes): Expr[ApplyUnapplyCodec[T]] = '{ ??? }
  def materializeRecursivelyImpl[T: Type](using Quotes): Expr[GenCodec[T]] = '{ ??? }
  def materializeImplicitlyImpl[T: Type](allow: Expr[AllowImplicitMacro[GenCodec[T]]])(using Quotes): Expr[GenCodec[T]] = '{ ??? }

  def refImpl[S: Type, T: Type](fun: Expr[S => T])(using Quotes): Expr[Nothing] = ???
  def fun2GenRefImpl[S: Type, T: Type](fun: Expr[S => T])(using Quotes): Expr[Nothing] = ???
}
