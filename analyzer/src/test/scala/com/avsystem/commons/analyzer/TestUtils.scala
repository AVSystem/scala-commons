package com.avsystem.commons
package analyzer

import com.avsystem.commons.annotation.{atLeast, explicitGenerics, macroPrivate}

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

  def genericMacroImpl[T](c: blackbox.Context)(arg: c.Tree): c.Tree = arg

  @explicitGenerics
  def genericMethod[T](arg: T): T = arg
  @explicitGenerics
  def genericMacro[T](arg: T): T = macro genericMacroImpl[T]

  @explicitGenerics
  class GenericClass[T]

  @explicitGenerics
  case class GenericCaseClass[T](arg: T)
}
