package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags

class ImplicitValueClasses extends AnalyzerRule {
  val name: String = "implicitValueClasses"

  override def transformTypeDef(tree: tpd.TypeDef)(using ctx: Context): tpd.Tree = {
    val sym = tree.symbol
    if (
      sym.isClass && sym.is(Flags.Implicit) && !sym.is(Flags.Synthetic) && !sym.is(Flags.Module) &&
      !sym.derivesFrom(ctx.definitions.AnyValClass)
    ) {
      report(tree, "implicit classes should extend AnyVal to avoid runtime overhead")
    }
    tree
  }
}
