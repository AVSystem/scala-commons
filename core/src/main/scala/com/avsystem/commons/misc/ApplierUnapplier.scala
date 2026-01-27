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
object Applier {
  given derived[T <: Product: Mirror.ProductOf as m]: Applier[T] = rawValues =>
    m.fromTuple(Tuple.fromArray(rawValues.toArray).asInstanceOf[m.MirroredElemTypes])
}

/**
 * Typeclass which captures case class `unapply`/`unapplySeq` method in a raw form that returns untyped sequence of
 * values.
 */
@implicitNotFound("cannot materialize Unapplier: ${T} is not a case class or case class like type")
trait Unapplier[T] {
  def unapply(value: T): Seq[Any]
}
object Unapplier {
  given derived[T <: Product]: Unapplier[T] = value => IArraySeq.unsafeWrapArray(value.productIterator.toArray)
}

@implicitNotFound("cannot materialize ApplierUnapplier: ${T} is not a case class or case class like type")
trait ApplierUnapplier[T] extends Applier[T] with Unapplier[T]
object ApplierUnapplier {
  given derived[T: {Applier as applier, Unapplier as unapplier}]: ApplierUnapplier[T] = new ApplierUnapplier[T] {
    override def apply(rawValues: Seq[Any]): T = applier.apply(rawValues)
    override def unapply(value: T): Seq[Any] = unapplier.unapply(value)
  }
}
