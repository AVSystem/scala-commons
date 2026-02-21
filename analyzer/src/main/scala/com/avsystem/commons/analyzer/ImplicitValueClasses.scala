package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.defn

class ImplicitValueClasses extends AnalyzerRule {
  val name: String = "implicitValueClasses"

  override def transformTypeDef(tree: tpd.TypeDef)(using Context): tpd.Tree = {
    val sym = tree.symbol
    if (
      sym.isClass && sym.is(Flags.Implicit) && !sym.is(Flags.Synthetic) && !sym.is(Flags.Module) &&
      !sym.derivesFrom(defn.AnyValClass)
    ) {
      report(tree, "implicit classes should extend AnyVal to avoid runtime overhead")
    }
    tree
  }
}
