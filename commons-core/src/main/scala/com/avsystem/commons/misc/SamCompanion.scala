package com.avsystem.commons
package misc

import com.avsystem.commons.misc.SamCompanion.ValidSam

abstract class SamCompanion[T, F](implicit vs: ValidSam[T, F]) {
  def apply(fun: F): T = macro com.avsystem.commons.macros.misc.SamMacros.toSam[T, F]
}

object SamCompanion {
  sealed trait ValidSam[T, F]
  object ValidSam {
    implicit def isValidSam[T, F]: ValidSam[T, F] = macro com.avsystem.commons.macros.misc.SamMacros.validateSam[T, F]
  }
}
