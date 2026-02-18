package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags

class FinalCaseClasses extends AnalyzerRule {
  val name: String = "finalCaseClasses"

  override def transformTypeDef(tree: tpd.TypeDef)(using Context): tpd.Tree = {
    val sym = tree.symbol
    // TypeDef is used for classes, traits, and type aliases in Scala 3 - filter for classes
    if (sym.isClass) {
      val flags = sym.flags
      val isCaseClass = flags.is(Flags.Case)
      val isFinal = flags.is(Flags.Final)
      val isSealed = flags.is(Flags.Sealed)
      // isStatic means top-level or nested in an object (not nested in a class/trait)
      val isStatic = sym.isStatic

      // Check if it's a case class that's not final and not sealed
      // For SI-4440: only report error for static (non-inner) case classes
      if (isCaseClass && !isFinal && !isSealed && isStatic) {
        report(tree, "Case classes should be marked as final")
      }
    }
    tree
  }
}
