package com.avsystem.commons
package misc

import com.avsystem.commons.macros.misc.MiscMacros

import scala.annotation.implicitNotFound

/** Macro materialized typeclass which captures the single value of a singleton type.
  */
@implicitNotFound("Cannot derive value of ${T} - is not a singleton type")
class ValueOf[T](val value: T) extends AnyVal
object ValueOf {
  def apply[T](implicit vof: ValueOf[T]): T = vof.value

  implicit def mkValueOf[T]: ValueOf[T] = macro MiscMacros.mkValueOf[T]

  inline implicit def mkValueOf[T]: ValueOf[T] = ${mkValueOfImpl[T]}
  def mkValueOfImpl[T](using Quotes, Type[T]): Expr[ValueOf[T]] = '{???}
}
