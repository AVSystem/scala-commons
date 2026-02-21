package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Symbols.defn

class ThrowableObjects extends AnalyzerRule {
  val name: String = "throwableObjects"

  override def transformTypeDef(tree: tpd.TypeDef)(using Context): tpd.Tree = {
    val sym = tree.symbol
    // Check if it's a module (object) class definition
    if (sym.is(Flags.Module) && sym.isClass) {
      // Check if the object extends Throwable (subtype check)
      if (sym.asClass.typeRef <:< defn.ThrowableType) {
        // Look up fillInStackTrace member on the object's type
        val fillInMember = sym.info.member(termName("fillInStackTrace"))

        // Check if the no-arg fillInStackTrace is still owned by Throwable (not overridden)
        // If all alternatives are owned by Throwable, the method hasn't been overridden
        val alternatives = fillInMember.alternatives
        val notOverridden = alternatives.nonEmpty && alternatives.forall(_.symbol.owner == defn.ThrowableClass)

        if (notOverridden) {
          report(tree, "objects should never extend Throwable unless they have no stack trace")
        }
      }
    }
    tree
  }
}
