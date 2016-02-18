package com.avsystem.commons
package analyzer

import com.avsystem.commons.annotation.{atLeast, macroPrivate}
import scala.reflect.macros.blackbox

/**
  * Author: ghik
  * Created: 08/09/15.
  */
object TestUtils {
  def need3Params(@atLeast(3) args: Any*) = ()

  @macroPrivate
  def macroPrivateMethod = 42

  def invokeMacroPrivateMethod: Int = macro invokeMacroPrivateMethodImpl

  def invokeMacroPrivateMethodImpl(c: blackbox.Context): c.Tree = {
    import c.universe._
    q"${c.prefix}.macroPrivateMethod"
  }
}
