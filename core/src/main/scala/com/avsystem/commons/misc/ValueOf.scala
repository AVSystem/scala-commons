package com.avsystem.commons
package misc

import scala.annotation.{implicitNotFound, nowarn}

/** Macro materialized typeclass which captures the single value of a singleton type.
  */
@implicitNotFound("Cannot derive value of ${T} - is not a singleton type")
@deprecated(
  "Use scala.ValueOf[T] from the standard library - it is auto-materialized by the compiler for singleton types since Scala 2.13",
  "2.28.0",
)
class ValueOf[T](val value: T) extends AnyVal {
  def toScala: scala.ValueOf[T] = new scala.ValueOf[T](value)
}

@nowarn("msg=deprecated")
object ValueOf {
  @deprecated("Use scala.valueOf[T] from the standard library (available since Scala 2.13)", "2.28.0")
  def apply[T](implicit vof: ValueOf[T]): T = vof.value
}
