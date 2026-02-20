package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Symbols.{NoSymbol, Symbol}

class CheckMacroPrivate extends AnalyzerRule {
  val name: String = "macroPrivate"

  private def macroPrivateAnnotClass(using Context): Symbol =
    Symbols.getClassIfDefined("com.avsystem.commons.annotation.macroPrivate")

  override def transformIdent(tree: tpd.Ident)(using Context): tpd.Tree = {
    checkMacroPrivate(tree)
    tree
  }

  override def transformSelect(tree: tpd.Select)(using Context): tpd.Tree = {
    checkMacroPrivate(tree)
    tree
  }

  private def checkMacroPrivate(tree: tpd.Tree)(using ctx: Context): Unit = {
    val annotCls = macroPrivateAnnotClass
    if (annotCls == NoSymbol) return

    val sym = tree.symbol
    if (sym == NoSymbol) return

    // Check if the referenced symbol (or any overridden version) has @macroPrivate
    val hasMacroPrivate = (sym :: sym.allOverriddenSymbols.toList)
      .exists(_.hasAnnotation(annotCls))

    if (hasMacroPrivate) {
      // Filter out definition sites: if the tree span is within the symbol's own definition span,
      // this is the definition itself, not a usage.
      if (isDefinitionSite(sym, tree)) return

      // Check if the usage is inside an inline method body (Scala 3 equivalent of macroExpandee)
      val insideInline = ctx.owner.ownersIterator.exists(_.isInlineMethod)
      if (!insideInline) {
        report(tree, s"${sym.name} is @macroPrivate and can only be used in inline (macro) code")
      }
    }
  }

  /**
   * Check if the tree is at the definition site of the symbol, not a usage site.
   * Prevents false positives when the MiniPhase visits internal references within
   * the definition itself.
   */
  private def isDefinitionSite(sym: Symbol, tree: tpd.Tree)(using Context): Boolean = {
    val treeSpan = tree.span
    if (!treeSpan.exists) return false

    val symSpan = sym.span
    symSpan.exists && symSpan.contains(treeSpan)
  }
}
