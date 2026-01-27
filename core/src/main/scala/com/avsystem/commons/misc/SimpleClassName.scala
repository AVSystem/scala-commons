package com.avsystem.commons
package misc


case class SimpleClassName[T](name: String) extends AnyVal
object SimpleClassName extends SimpleClassNameMacros {
  def of[T](using scn: SimpleClassName[T]): String = scn.name
}
