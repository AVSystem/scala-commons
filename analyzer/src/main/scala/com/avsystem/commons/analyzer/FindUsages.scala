package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol

class FindUsages extends AnalyzerRule {
  val name: String = "findUsages"

  private lazy val rejectedSymbols: Set[String] =
    argument.map(_.split(";").toSet).getOrElse(Set.empty)

  override def transformIdent(tree: tpd.Ident)(using Context): tpd.Tree = {
    checkTree(tree)
    tree
  }

  override def transformSelect(tree: tpd.Select)(using Context): tpd.Tree = {
    checkTree(tree)
    tree
  }

  override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree = {
    checkTree(tree)
    tree
  }

  override def transformNew(tree: tpd.New)(using Context): tpd.Tree = {
    checkTree(tree)
    tree
  }

  override def transformOther(tree: tpd.Tree)(using Context): tpd.Tree = {
    checkTree(tree)
    tree
  }

  private def checkTree(tree: tpd.Tree)(using Context): Unit = {
    if (rejectedSymbols.nonEmpty && tree.symbol != NoSymbol) {
      val fullName = tree.symbol.fullName.toString
      if (rejectedSymbols.contains(fullName)) {
        report(tree, s"found usage of $fullName")
      }
    }
  }
}
