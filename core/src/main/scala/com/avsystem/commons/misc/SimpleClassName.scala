package com.avsystem.commons
package misc

import scala.quoted.*

case class SimpleClassName[T](name: String) extends AnyVal
object SimpleClassName {
  def of[T](implicit scn: SimpleClassName[T]): String = scn.name

  inline implicit def materialize[T]: SimpleClassName[T] = ${ materializeImpl[T] }

  private def materializeImpl[T: Type](using Quotes): Expr[SimpleClassName[T]] = {
    import quotes.reflect.*
    val sym = TypeRepr.of[T].dealias.typeSymbol
    val name = Expr(sym.name.stripSuffix("$"))
    '{ SimpleClassName[T]($name) }
  }
}
