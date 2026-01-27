package com.avsystem.commons
package misc

import scala.annotation.implicitNotFound

/**
 * Macro materialized typeclass which captures the single value of a singleton type.
 */
@implicitNotFound("Cannot derive value of ${T} - is not a singleton type")
class ValueOf[T](val value: T) extends AnyVal
object ValueOf {
  inline given [T] => ValueOf[T] = ${ mkValueOfImpl[T] }
  def apply[T](using vof: ValueOf[T]): T = vof.value
  def mkValueOfImpl[T: Type](using Quotes): Expr[ValueOf[T]] = '{ ??? }.asInstanceOf[Expr[ValueOf[T]]]
}
