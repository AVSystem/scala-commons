package com.avsystem.commons
package macros

import scala.reflect.macros.blackbox

/**
  * Author: ghik
  * Created: 07/01/16.
  */
class UniversalMacros(val c: blackbox.Context) {

  import c.universe._

  def showAst[A]: Tree = {
    val Apply(_, List(prefix)) = c.prefix.tree
    c.error(prefix.pos, showCode(prefix))
    prefix
  }

  def showRawAst[A]: Tree = {
    val Apply(_, List(prefix)) = c.prefix.tree
    c.error(prefix.pos, showRaw(prefix))
    prefix
  }

  def showSymbol[A]: Tree = {
    val Apply(_, List(prefix)) = c.prefix.tree
    c.error(prefix.pos, show(prefix.symbol))
    prefix
  }

  def showSymbolFullName[A]: Tree = {
    val Apply(_, List(prefix)) = c.prefix.tree
    c.error(prefix.pos, prefix.symbol.fullName)
    prefix
  }

  def showType[A]: Tree = {
    val Apply(_, List(prefix)) = c.prefix.tree
    c.error(prefix.pos, showCode(tq"${prefix.tpe}"))
    prefix
  }

  def showRawType[A]: Tree = {
    val Apply(_, List(prefix)) = c.prefix.tree
    c.error(prefix.pos, showRaw(prefix.tpe))
    prefix
  }

  def showTypeSymbol[A]: Tree = {
    val Apply(_, List(prefix)) = c.prefix.tree
    c.error(prefix.pos, show(prefix.tpe.typeSymbol))
    prefix
  }

  def showTypeSymbolFullName[A]: Tree = {
    val Apply(_, List(prefix)) = c.prefix.tree
    c.error(prefix.pos, prefix.tpe.typeSymbol.fullName)
    prefix
  }
}
