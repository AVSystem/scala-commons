package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Symbols.{NoSymbol, Symbol}

class ShowAst extends AnalyzerRule {
  val name: String = "showAst"
  level = Level.Error

  private def showAstAnnotClass(using Context): Symbol =
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
    val annotCls = showAstAnnotClass
    if (annotCls != NoSymbol && tree.symbol != NoSymbol) {
      if (tree.symbol.annotations.exists(_.symbol == annotCls)) {
        report(tree, tree.show)
      }
    }
  }
}
