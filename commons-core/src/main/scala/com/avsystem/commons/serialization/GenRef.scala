package com.avsystem.commons
package serialization

import scala.language.implicitConversions

sealed trait RawRef {

  import RawRef._

  def normalize: Iterator[SimpleRawRef] = this match {
    case Identity => Iterator.empty
    case s: SimpleRawRef => Iterator(s)
    case Composite(left, right) => left.normalize ++ right.normalize
  }
}

sealed trait SimpleRawRef extends RawRef

object RawRef {
  case class Field(name: String) extends SimpleRawRef
  case class Composite(left: RawRef, right: RawRef) extends RawRef
  case object Identity extends RawRef

  implicit val codec: GenCodec[RawRef] = GenCodec.materialize[RawRef]

  def create[S]: Creator[S] = new Creator[S] {}

  trait Creator[S] {
    def ref[T](fun: S => T): RawRef = macro macros.serialization.GenRefMacros.rawRef
  }
}

object SimpleRawRef {
  implicit val codec: GenCodec[SimpleRawRef] = GenCodec.materialize[SimpleRawRef]
}

case class GenRef[-S, +T](fun: S => T, rawRef: RawRef) {
  def apply(s: S): T = fun(s)

  def andThen[T0](other: GenRef[T, T0]): GenRef[S, T0] =
    GenRef(fun andThen other.fun, RawRef.Composite(rawRef, other.rawRef))

  def compose[S0](other: GenRef[S0, S]): GenRef[S0, T] =
    other andThen this
}

object GenRef {
  def identity[S]: GenRef[S, S] = GenRef(s => s, RawRef.Identity)
  def create[S]: Creator[S] = new Creator[S] {}

  trait Creator[S] {
    type Ref[T] = GenRef[S, T]

    def ref[T](fun: S => T): GenRef[S, T] = macro macros.serialization.GenRefMacros.genRef
  }

  trait Implicits {
    implicit def fun2GenRef[S, T](fun: S => T): GenRef[S, T] = macro macros.serialization.GenRefMacros.genRef
  }
  object Implicits extends Implicits
}
