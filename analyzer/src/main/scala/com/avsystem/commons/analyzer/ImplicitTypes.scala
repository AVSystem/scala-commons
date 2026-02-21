package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags

/**
 * Warns when implicit val/def or named given definitions have inferred
 * (missing) type annotations. Explicit type annotations on implicit/given
 * definitions improve code clarity and prevent accidental type widening.
 *
 * Does NOT warn on:
 *  - anonymous givens (their type is inherently declared)
 *  - non-implicit/non-given definitions
 *  - compiler-generated (Synthetic) definitions
 *  - parameters (they have different semantics)
 */
class ImplicitTypes extends AnalyzerRule {
  val name: String = "implicitTypes"

  override def transformValDef(tree: tpd.ValDef)(using Context): tpd.Tree = {
    checkImplicitType(tree, tree.tpt)
    tree
  }

  override def transformDefDef(tree: tpd.DefDef)(using Context): tpd.Tree = {
    checkImplicitType(tree, tree.tpt)
    tree
  }

  private def checkImplicitType(tree: tpd.MemberDef, tpt: tpd.Tree)(using Context): Unit = {
    val sym = tree.symbol
    if (sym.isOneOf(Flags.GivenOrImplicit) && !sym.is(Flags.Synthetic) && !sym.is(Flags.Param)) {
      // Detect inferred type: when the compiler infers a type, the TypeTree's span
      // is either non-existent or not source-derived (synthetic span).
      val typeInferred = !tpt.span.exists || !tpt.span.isSourceDerived
      if (typeInferred) {
        report(tree, "implicit/given definitions should have an explicit type annotation")
      }
    }
  }
}
