package com.avsystem.commons
package misc

case class SimpleClassName[T](name: String) extends AnyVal
object SimpleClassName extends SimpleClassNameMacros {
  def of[T](implicit scn: SimpleClassName[T]): String = scn.name
}
