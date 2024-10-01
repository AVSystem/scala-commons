package com.avsystem.commons
package analyzer

import com.avsystem.commons.annotation.explicitGenerics

import scala.quoted.{Expr, Quotes}

object TestUtils:
  //  def need3Params(@atLeast(3) args: Any*) = ()
  //
  //  @macroPrivate
  //  def macroPrivateMethod = 42
  def genericMacroImpl[T](arg: Expr[T])(using Quotes): Expr[T] = arg
  @explicitGenerics
  def genericMethod[T](arg: T): T = arg
  //  def invokeMacroPrivateMethod: Int = macro invokeMacroPrivateMethodImpl
  //  def invokeMacroPrivateMethodImpl(c: blackbox.Context): c.Tree =
  //    import c.universe.*
  //    q"${c.prefix}.macroPrivateMethod"
  //
  //  end invokeMacroPrivateMethodImpl
  @explicitGenerics
  inline def genericMacro[T](arg: T): T = ${ genericMacroImpl[T]('{ arg }) }
  //  object Extractor:
  //    @macroPrivate def unapply(any: Any): Option[Any] = None
  //
  //  end Extractor

end TestUtils
