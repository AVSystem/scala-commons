package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Symbols.{NoSymbol, Symbol}

class ShowAst(using Context) extends AnalyzerRule {
  val name: String = "showAst"
  level = Level.Error

  private val showAstAnnotClass: Symbol =
    Symbols.getClassIfDefined("com.avsystem.commons.annotation.showAst")

  override def transformValDef(tree: tpd.ValDef)(using Context): tpd.Tree = {
    checkShowAst(tree)
    tree
  }

  override def transformDefDef(tree: tpd.DefDef)(using Context): tpd.Tree = {
    checkShowAst(tree)
    tree
  }

  override def transformTypeDef(tree: tpd.TypeDef)(using Context): tpd.Tree = {
    checkShowAst(tree)
    tree
  }

  private def checkShowAst(tree: tpd.MemberDef)(using Context): Unit = {
    if (showAstAnnotClass != NoSymbol && tree.symbol != NoSymbol) {
      if (tree.symbol.annotations.exists(_.symbol == showAstAnnotClass)) {
        report(tree, tree.show)
      }
    }
  }
}
