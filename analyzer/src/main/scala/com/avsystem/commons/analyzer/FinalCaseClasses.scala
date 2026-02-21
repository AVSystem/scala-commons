package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags

class FinalCaseClasses extends AnalyzerRule {
  val name: String = "finalCaseClasses"

  override def transformTypeDef(tree: tpd.TypeDef)(using Context): tpd.Tree = {
    if (tree.isClassDef) {
      val flags = tree.symbol.flags
      if (flags.is(Flags.Case) && !flags.is(Flags.Final) && !flags.is(Flags.Sealed)) {
        report(tree, "Case classes should be marked as final")
      }
    }
    tree
  }
}
