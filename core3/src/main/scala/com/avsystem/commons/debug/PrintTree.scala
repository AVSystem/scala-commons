package com.avsystem.commons
package debug

import scala.quoted.{Expr, Quotes, Type}

object PrintTree {
  inline def printTree[T](inline x: T): Unit = ${printTreeImpl('x)}
  def printTreeImpl[T: Type](x: Expr[T])(using quotes: Quotes): Expr[Unit] =
    import quotes.reflect.*
    println(x.asTerm.show(using Printer.TreeStructure))
    '{()}
}
