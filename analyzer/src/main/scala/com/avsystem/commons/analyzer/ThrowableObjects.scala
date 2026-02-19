package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.termName

class ThrowableObjects extends AnalyzerRule {
  val name: String = "throwableObjects"

  override def transformTypeDef(tree: tpd.TypeDef)(using ctx: Context): tpd.Tree = {
    val sym = tree.symbol
    // Check if it's a module (object) class definition
    if (sym.is(Flags.Module) && sym.isClass) {
      val tpe = sym.asClass.typeRef
      val throwableTpe = ctx.definitions.ThrowableClass.typeRef

      // Check if the object extends Throwable (subtype check)
      if (tpe <:< throwableTpe) {
        val throwableSym = ctx.definitions.ThrowableClass

        // Look up fillInStackTrace member on the object's type
        val fillInMember = sym.info.member(termName("fillInStackTrace"))

        // Check if the no-arg fillInStackTrace is still owned by Throwable (not overridden)
        // If all alternatives are owned by Throwable, the method hasn't been overridden
        val alternatives = fillInMember.alternatives
        val notOverridden = alternatives.nonEmpty && alternatives.forall(_.symbol.owner == throwableSym)

        if (notOverridden) {
          report(tree, "objects should never extend Throwable unless they have no stack trace")
        }
      }
    }
    tree
  }
}
