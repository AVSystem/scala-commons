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

abstract class TransparentWrapperCompanion[R, T] extends TransparentWrapping[T] {
  type Wrapped = R

  implicit def self: this.type = this

  def apply(r: R): T
  def unapply(t: T): Option[R]
}

abstract class StringWrapperCompanion[T] extends TransparentWrapperCompanion[String, T]
abstract class IntWrapperCompanion[T] extends TransparentWrapperCompanion[Int, T]
abstract class LongWrapperCompanion[T] extends TransparentWrapperCompanion[Long, T]
