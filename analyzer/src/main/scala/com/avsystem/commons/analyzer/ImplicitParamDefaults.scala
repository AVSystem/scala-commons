package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags

class ImplicitParamDefaults extends AnalyzerRule {
  val name: String = "implicitParamDefaults"

  override def transformDefDef(tree: tpd.DefDef)(using Context): tpd.Tree = {
    tree.termParamss.zipWithIndex.foreach { (paramList, idx) =>
      paramList.foreach { param =>
        if (param.symbol.isOneOf(Flags.GivenOrImplicit) && param.symbol.is(Flags.HasDefault)) {
          report(param, "Implicit parameters should not have default values")
        }
      }
    }
    tree
  }
}
