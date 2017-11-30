package com.avsystem.commons
package misc

object Implicits {
  def infer[T](clue: String): T = macro macros.misc.MiscMacros.infer[T]
  def inferNonMacro[T](clue: String): T = macro macros.misc.MiscMacros.inferNonMacro[T]
}
