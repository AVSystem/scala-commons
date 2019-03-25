package com.avsystem.commons
package serialization

import com.avsystem.commons.annotation.bincompat

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
  * Base class for companion objects of case classes which are transparent wrappers over their only field.
  * This is the usual way of providing [[TransparentWrapping]] for some type and is intended as a replacement for
  * [[transparent]] annotation.
  */
abstract class TransparentWrapperCompanion[R, T] extends TransparentWrapping[R, T] {
  @bincompat private[this] var transformedCodec: GenCodec[T] = _

  @bincompat private[commons] def this(wrappedCodec: GenCodec[R]) = {
    this()
    transformedCodec = new GenCodec.Transformed(wrappedCodec, unwrap, wrap)
  }

  implicit def self: this.type = this

  def apply(r: R): T
  def unapply(t: T): Option[R]

  final def wrap(r: R): T = apply(r)
  final def unwrap(t: T): R = unapply(t).getOrElse(throw new NoSuchElementException(s"unapply for $t failed"))

  @bincompat private[commons] def codec: GenCodec[T] = transformedCodec
}

abstract class StringWrapperCompanion[T] extends TransparentWrapperCompanion[String, T]
abstract class IntWrapperCompanion[T] extends TransparentWrapperCompanion[Int, T]
abstract class LongWrapperCompanion[T] extends TransparentWrapperCompanion[Long, T]
