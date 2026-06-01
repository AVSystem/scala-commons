package com.avsystem.commons
package misc

import com.avsystem.commons.misc.SamCompanion.ValidSam

@deprecated(
  "Use native SAM conversion instead, e.g. `val r: Runnable = () => doStuff()` or `val c: JConsumer[T] = t => ...`",
  "2.28.0",
)
abstract class SamCompanion[T, F](implicit vs: ValidSam[T, F]) {
  // TODO[scala3-port]: SamCompanion.apply (Scala 2 macro def) (L)
  def apply(fun: F): T = ???
}

object SamCompanion {
  sealed trait ValidSam[T, F]

  object ValidSam {
    // TODO[scala3-port]: isValidSam (Scala 2 macro def) (L)
    given isValidSam[T, F]: ValidSam[T, F] = ???
  }
}
