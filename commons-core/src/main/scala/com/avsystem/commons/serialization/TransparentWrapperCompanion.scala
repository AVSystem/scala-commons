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
}

abstract class StringWrapperCompanion[T] extends TransparentWrapperCompanion[String, T]
abstract class IntWrapperCompanion[T] extends TransparentWrapperCompanion[Int, T]
abstract class LongWrapperCompanion[T] extends TransparentWrapperCompanion[Long, T]
