package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Symbols.{NoSymbol, Symbol}

class ExplicitGenerics(using Context) extends AnalyzerRule {
  val name: String = "explicitGenerics"

  private val explicitGenericsAnnotClass: Symbol =
    Symbols.getClassIfDefined("com.avsystem.commons.annotation.explicitGenerics")

  override def transformTypeApply(tree: tpd.TypeApply)(using Context): tpd.Tree = {
    if (explicitGenericsAnnotClass != NoSymbol && tree.fun.symbol != NoSymbol) {
      val sym = tree.fun.symbol
      val hasAnnot = (sym :: sym.allOverriddenSymbols.toList)
        .exists(_.hasAnnotation(explicitGenericsAnnotClass))

      if (hasAnnot) {
        // Inferred type args in Scala 3 have their span set to the fun's span
        // (identical start/end), while explicit type args have distinct spans
        // that come after the fun span (inside the [T1, T2] brackets).
        val funSpan = tree.fun.span
        val allInferred = tree.args.forall { arg =>
          val s = arg.span
          !s.exists || (s.start == funSpan.start && s.end == funSpan.end)
        }
        if (allInferred) {
          report(tree, s"${sym.name} requires explicit type arguments")
        }
      }
    }
    tree
  }
}
