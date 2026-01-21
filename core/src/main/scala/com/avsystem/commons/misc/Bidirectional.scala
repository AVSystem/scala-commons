package com.avsystem.commons
package misc

/** Creates reversed partial function. */
object Bidirectional {
  def apply[A, B](pf: PartialFunction[A, B]): (PartialFunction[A, B], PartialFunction[B, A]) = macro com.avsystem.commons.macros.misc.BidirectionalMacro.impl[A, B]
  inline def apply[A, B](pf: PartialFunction[A, B]): (PartialFunction[A, B], PartialFunction[B, A]) = ${impl[A, B]( 'pf )}
  def impl[A: Type, B: Type](pf: Expr[PartialFunction[A, B]])(using Quotes): Expr[(PartialFunction[A, B], PartialFunction[B, A])] = '{???}

}
