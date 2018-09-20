package com.avsystem.commons
package misc

import com.avsystem.commons.macros.misc.MiscMacros

import scala.annotation.implicitNotFound

@implicitNotFound("${T} is not a singleton type")
class ValueOf[T](val value: T) extends AnyVal
object ValueOf {
  def apply[T](implicit vof: ValueOf[T]): T = vof.value

  implicit def mkValueOf[T]: ValueOf[T] = macro MiscMacros.mkValueOf[T]
}
