package com.avsystem.commons
package misc

/** Creates reversed partial function. */
object Bidirectional {
  def apply[A, B](pf: PartialFunction[A, B]): (PartialFunction[A, B], PartialFunction[B, A]) =
    macro com.avsystem.commons.macros.misc.BidirectionalMacro.impl[A, B]
}
