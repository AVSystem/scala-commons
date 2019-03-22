package com.avsystem.commons
package serialization

trait TransparentWrapping[T] {
  type Wrapped
  def apply(r: Wrapped): T
  def unapply(t: T): Option[Wrapped]
}
object TransparentWrapping {
  // Type member for wrapped type and Aux pattern is a workaround for divergent implicit expansion
  // when TransparentWrapping is used as implicit
  type Aux[R, T] = TransparentWrapping[T] {type Wrapped = R}
}

abstract class TransparentWrapperCompanion[R: GenCodec, T] extends TransparentWrapping[T] {
  type Wrapped = R

  implicit def self: this.type = this

  def apply(r: R): T
  def unapply(t: T): Option[R]

  // TODO: in 1.35.0 remove and replace with `fromTransparentWrapper` implicit in `GenCodec` object in
  implicit val codec: GenCodec[T] =
    new GenCodec.Transformed[T, R](GenCodec[R], t => unapply(t).get, apply)
}

abstract class StringWrapperCompanion[T] extends TransparentWrapperCompanion[String, T]
abstract class IntWrapperCompanion[T] extends TransparentWrapperCompanion[Int, T]
abstract class LongWrapperCompanion[T] extends TransparentWrapperCompanion[Long, T]
