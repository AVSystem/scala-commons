package com.avsystem.commons
package analyzer

import com.avsystem.commons.annotation.{atLeast, macroPrivate}

import scala.reflect.macros.blackbox

object TestUtils {
  def need3Params(@atLeast(3) args: Any*) = ()

  @macroPrivate
  def macroPrivateMethod = 42

  def invokeMacroPrivateMethod: Int = macro invokeMacroPrivateMethodImpl

  def invokeMacroPrivateMethodImpl(c: blackbox.Context): c.Tree = {
    import c.universe._
    q"${c.prefix}.macroPrivateMethod"
  }

  object Extractor {
    @macroPrivate def unapply(any: Any): Option[Any] = None
  }
}
