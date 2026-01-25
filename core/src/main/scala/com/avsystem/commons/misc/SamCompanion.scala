package com.avsystem.commons
package misc

import com.avsystem.commons.misc.SamCompanion.ValidSam

abstract class SamCompanion[T, F](implicit vs: ValidSam[T, F]) extends SamCompanionMacros[T, F]

object SamCompanion {
  sealed trait ValidSam[T, F]
  object ValidSam extends ValidSamMacros
}
