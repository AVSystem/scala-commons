package com.avsystem.commons
package misc

import scala.annotation.implicitNotFound

/**
 * Typeclass which captures case class `apply` method in a raw form that takes untyped sequence of arguments.
 */
@implicitNotFound("cannot materialize Applier: ${T} is not a case class or case class like type")
trait Applier[T] {
  def apply(rawValues: Seq[Any]): T
}
object Applier extends ApplierMacros

/**
 * Typeclass which captures case class `unapply`/`unapplySeq` method in a raw form that returns untyped sequence of
 * values.
 */
@implicitNotFound("cannot materialize Unapplier: ${T} is not a case class or case class like type")
trait Unapplier[T] {
  def unapply(value: T): Seq[Any]
}
object Unapplier extends UnapplierMacros

class ProductUnapplier[T <: Product] extends Unapplier[T] {
  def unapply(value: T): Seq[Any] = IArraySeq.unsafeWrapArray(value.productIterator.toArray)
}
abstract class ProductApplierUnapplier[T <: Product] extends ProductUnapplier[T] with ApplierUnapplier[T]

@implicitNotFound("cannot materialize ApplierUnapplier: ${T} is not a case class or case class like type")
trait ApplierUnapplier[T] extends Applier[T] with Unapplier[T]
object ApplierUnapplier extends ApplierUnapplierMacros
