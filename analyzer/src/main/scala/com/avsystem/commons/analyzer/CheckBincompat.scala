package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Symbols.{NoSymbol, Symbol}

class CheckBincompat(using Context) extends AnalyzerRule {
  val name: String = "bincompat"

  private val bincompatAnnotClass: Symbol =
    Symbols.getClassIfDefined("com.avsystem.commons.annotation.bincompat")

  override def transformIdent(tree: tpd.Ident)(using Context): tpd.Tree = {
    checkTree(tree)
    tree
  }

  override def transformSelect(tree: tpd.Select)(using Context): tpd.Tree = {
    checkTree(tree)
    tree
  }

  override def transformNew(tree: tpd.New)(using Context): tpd.Tree = {
    checkTree(tree)
    tree
  }

  private def checkTree(tree: tpd.Tree)(using Context): Unit = {
    if (tree.symbol != NoSymbol && bincompatAnnotClass != NoSymbol) {
      val sym = tree.symbol
      val hasAnnot = sym.annotations.exists(annot => annot.symbol == bincompatAnnotClass)
      if (hasAnnot && !isDefinitionSite(sym, tree)) {
        report(
          tree,
          "Symbols annotated as @bincompat exist only for binary compatibility and should not be used directly",
        )
      }
    }
  }

  /**
   * Check if the tree is at the definition site of the symbol, not a usage site.
   * In Scala 3, the MiniPhase may see internal references within a TypeDef/ValDef
   * that correspond to the definition itself (e.g., module class references inside
   * an object definition). We detect this by checking whether:
   * 1. The symbol's definition span contains the tree's span, OR
   * 2. The companion module/class definition span contains the tree's span
   *    (handles `object X` where the module class Ident appears inside the ValDef).
   */
  private def isDefinitionSite(sym: Symbol, tree: tpd.Tree)(using Context): Boolean = {
    val treeSpan = tree.span
    if (!treeSpan.exists) return false

    def spanContains(s: Symbol): Boolean = {
      val sp = s.span
      sp.exists && sp.contains(treeSpan)
    }

    spanContains(sym) || {
      // For module classes, also check the module val (companion module)
      // For module vals, also check the module class
      val companion =
        if (sym.is(dotty.tools.dotc.core.Flags.ModuleClass)) sym.companionModule
        else if (sym.is(dotty.tools.dotc.core.Flags.Module)) sym.companionClass
        else NoSymbol
      companion != NoSymbol && spanContains(companion)
    }
  }
}
