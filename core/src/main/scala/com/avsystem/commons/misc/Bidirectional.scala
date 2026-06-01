package com.avsystem.commons
package misc

/** Creates reversed partial function. */
object Bidirectional {
  // TODO[scala3-port]: apply (Scala 2 macro def) (L)
  def apply[A, B](pf: PartialFunction[A, B]): (PartialFunction[A, B], PartialFunction[B, A]) = ???
}
