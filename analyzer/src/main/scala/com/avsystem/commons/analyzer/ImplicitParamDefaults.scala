package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags

class ImplicitParamDefaults extends AnalyzerRule {
  val name: String = "implicitParamDefaults"

  override def transformDefDef(tree: tpd.DefDef)(using ctx: Context): tpd.Tree = {
    tree.termParamss.zipWithIndex.foreach { case (paramList, idx) =>
      paramList.foreach { param =>
        val isImplicitOrGiven = param.symbol.is(Flags.Implicit) || param.symbol.is(Flags.Given)
        // Check if this parameter has a default value by looking for $default method on owner
        val hasDefault = param.symbol.is(Flags.HasDefault)
        if (isImplicitOrGiven && hasDefault) {
          report(param, "Implicit parameters should not have default values")
        }
      }
    }
    tree
  }
}
