package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags

class FinalValueClasses extends AnalyzerRule {
  val name: String = "finalValueClasses"

  override def transformTypeDef(tree: tpd.TypeDef)(using ctx: Context): tpd.Tree = {
    val sym = tree.symbol
    // In Scala 3, the compiler automatically adds the Final flag to all value classes
    // (classes extending AnyVal) during an early phase, so by the time our plugin phase
    // runs after typer, all AnyVal subclasses already have Flags.Final set.
    // This makes the rule effectively a no-op in Scala 3, unlike Scala 2 where the
    // user had to explicitly write `final`. We keep the rule for forward compatibility.
    if (sym.isClass && !sym.flags.is(Flags.Final)) {
      val anyValClass = ctx.definitions.AnyValClass
      if (sym.derivesFrom(anyValClass) && sym != anyValClass) {
        report(tree, "Value classes should be marked as final")
      }
    }
    tree
  }
}
