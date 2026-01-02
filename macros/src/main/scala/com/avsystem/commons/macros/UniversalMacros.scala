package com.avsystem.commons
package macros

import scala.reflect.macros.blackbox

class UniversalMacros(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  import c.universe._

  def sourceCode: Tree = {
    if (!ctx.compilerSettings.contains("-Yrangepos")) {
      abort("sourceCode only works with range positions enabled (-Yrangepos)")
    }
    val Apply(_, List(prefix)) = c.prefix.tree
    val pos = prefix.pos
    val code = new String(pos.source.content, pos.start, pos.end - pos.start)

    def lineIndent(line: String): Int =
      line.indexWhere(c => !c.isWhitespace) match {
        case -1 => 0
        case i => i
      }

    val indentToStrip = lineIndent(pos.source.lineToString(pos.source.offsetToLine(pos.start)))
    val stripped = code.split('\n').map(l => l.drop(indentToStrip.min(lineIndent(l)))).mkString("\n")
    q"$stripped"
  }

  def withSourceCode: Tree = {
    val Apply(_, List(prefix)) = c.prefix.tree
    q"($prefix, $sourceCode)"
  }

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
    c.error(prefix.pos, showCode(tq"${prefix.tpe.widen}"))
    prefix
  }

  def showRawType[A]: Tree = {
    val Apply(_, List(prefix)) = c.prefix.tree
    c.error(prefix.pos, showRaw(prefix.tpe.widen))
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
