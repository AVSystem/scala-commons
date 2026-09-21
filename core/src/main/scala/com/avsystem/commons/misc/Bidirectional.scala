package com.avsystem.commons
package misc

/** Creates reversed partial function.
  *
  * @deprecated
  *   `Bidirectional` is being removed in the Scala 3 release of scala-commons. The whitebox macro has no Scala 3
  *   counterpart; write the reversed `PartialFunction` manually at call sites.
  */
@deprecated(
  "Bidirectional will be removed in the Scala 3 release of scala-commons. Write the reversed PartialFunction manually.",
  since = "2.29.0",
)
object Bidirectional {
  def apply[A, B](pf: PartialFunction[A, B]): (PartialFunction[A, B], PartialFunction[B, A]) =
    macro com.avsystem.commons.macros.misc.BidirectionalMacro.impl[A, B]
}
