package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.defn

class CatchThrowable extends AnalyzerRule {
  val name: String = "catchThrowable"

  override def transformTry(tree: tpd.Try)(using Context): tpd.Tree = {
    tree.cases.foreach { caseDef =>
      // Skip cases without source position or with synthetic spans (generated from custom handlers)
      if (caseDef.span.exists && caseDef.span.isSourceDerived) {
        checkPattern(caseDef.pat, caseDef)
      }
    }
    tree
  }

  private def checkPattern(pat: tpd.Tree, caseDef: tpd.CaseDef)(using Context): Unit = {
    pat match {
      // Handle simple Bind patterns: case t: Throwable
      case tpd.Bind(_, body) =>
        checkPattern(body, caseDef)

      // Handle Typed patterns: t: Throwable
      case tpd.Typed(_, tpt) =>
        if (tpt.tpe =:= defn.ThrowableType && !isCustomExtractor(pat)) {
          report(caseDef, "Catching Throwable is discouraged, catch specific exceptions instead")
        }

      // Handle Alternative patterns: case _: A | _: B
      case tpd.Alternative(trees) =>
        trees.foreach(t => checkPattern(t, caseDef))

      // Handle direct type patterns
      case _ =>
        if (pat.tpe =:= defn.ThrowableType && !isCustomExtractor(pat)) {
          report(caseDef, "Catching Throwable is discouraged, catch specific exceptions instead")
        }
    }
  }

  private def isCustomExtractor(tree: tpd.Tree)(using Context): Boolean = tree match {
    case _: tpd.UnApply => true
    case _ => false
  }
}
