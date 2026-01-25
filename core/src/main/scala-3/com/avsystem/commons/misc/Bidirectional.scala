package com.avsystem.commons
package misc

object Bidirectional {
  inline def apply[A, B](pf: PartialFunction[A, B]): (PartialFunction[A, B], PartialFunction[B, A]) = ${ impl[A, B]('pf) }
  def impl[A: Type, B: Type](pf: Expr[PartialFunction[A, B]])(using Quotes)
    : Expr[(PartialFunction[A, B], PartialFunction[B, A])] = '{ ??? }
}
