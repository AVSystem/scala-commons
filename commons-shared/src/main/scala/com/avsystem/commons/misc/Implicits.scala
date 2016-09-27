package com.avsystem.commons
package misc

/**
  * Author: ghik
  * Created: 19/09/16.
  */
object Implicits {
  def infer[T](clue: String): T = macro macros.misc.MiscMacros.infer[T]
  def inferNonMacro[T](clue: String): T = macro macros.misc.MiscMacros.inferNonMacro[T]
}
