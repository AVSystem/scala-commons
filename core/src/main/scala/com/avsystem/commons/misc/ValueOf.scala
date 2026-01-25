package com.avsystem.commons
package misc

import scala.annotation.implicitNotFound

/**
 * Macro materialized typeclass which captures the single value of a singleton type.
 */
@implicitNotFound("Cannot derive value of ${T} - is not a singleton type")
class ValueOf[T](val value: T) extends AnyVal
object ValueOf extends ValueOfMacros {
  def apply[T](implicit vof: ValueOf[T]): T = vof.value
}
