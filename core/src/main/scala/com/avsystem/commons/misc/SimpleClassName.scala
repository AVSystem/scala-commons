package com.avsystem.commons
package misc

case class SimpleClassName[T](name: String) extends AnyVal
object SimpleClassName {
  def of[T](implicit scn: SimpleClassName[T]): String = scn.name

  // TODO[scala3-port]: SimpleClassName.materialize (Scala 2 macro def) (L)
  given materialize[T]: SimpleClassName[T] = ???
}
