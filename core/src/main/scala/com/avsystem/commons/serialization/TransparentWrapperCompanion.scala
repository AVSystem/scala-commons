package com.avsystem.commons
package serialization

/**
  * A typeclass which serves as evidence that some type `T` is a "transparent" wrapper of some other type.
  * This usually means that instances of various typeclasses (e.g. [[GenCodec]]) for type `T` could be automatically
  * derived from instances for the wrapped type. How this actually happens is decided in each typeclass which can
  * define appropriate implicit.
  */
trait TransparentWrapping[R, T] {
  def wrap(r: R): T
  def unwrap(t: T): R
}
object TransparentWrapping {
  private val reusableIdentity = new TransparentWrapping[Any, Any] {
    def wrap(r: Any): Any = r
    def unwrap(t: Any): Any = t
  }

  // unfortunately can't make this implicit, the compiler is not good enough and gets lost in implicit divergence
  def identity[T]: TransparentWrapping[T, T] =
    reusableIdentity.asInstanceOf[TransparentWrapping[T, T]]
}

/**
  * Base class for companion objects of case classes which are transparent wrappers ("newtypes") over their only field.
  * This is the usual way of providing [[TransparentWrapping]] for some type and is intended as a replacement for
  * [[transparent]] annotation where possible.
  */
abstract class TransparentWrapperCompanion[R, T] extends TransparentWrapping[R, T] with (R => T) {
  implicit def self: TransparentWrapping[R, T] = this

  def apply(r: R): T
  def unapply(t: T): Option[R]

  final def wrap(r: R): T = apply(r)
  final def unwrap(t: T): R = unapply(t).getOrElse(throw new NoSuchElementException(s"unapply for $t failed"))

  implicit def ordering(implicit wrappedOrdering: Ordering[R]): Ordering[T] =
    Ordering.by(unwrap)
}

abstract class StringWrapperCompanion[T] extends TransparentWrapperCompanion[String, T]
abstract class IntWrapperCompanion[T] extends TransparentWrapperCompanion[Int, T]
abstract class LongWrapperCompanion[T] extends TransparentWrapperCompanion[Long, T]
abstract class FloatWrapperCompanion[T] extends TransparentWrapperCompanion[Float, T]
abstract class DoubleWrapperCompanion[T] extends TransparentWrapperCompanion[Double, T]
abstract class BooleanWrapperCompanion[T] extends TransparentWrapperCompanion[Boolean, T]
