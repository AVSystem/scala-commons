package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.Type

class ImplicitFunctionParams extends AnalyzerRule {
  val name: String = "implicitFunctionParams"

  override def transformDefDef(tree: tpd.DefDef)(using Context): tpd.Tree = {
    tree.termParamss.foreach { paramList =>
      paramList.foreach { param =>
        if (param.symbol.isOneOf(Flags.GivenOrImplicit) && isFunctionLikeType(param.tpt.tpe)) {
          report(
            param,
            "implicit/using parameter should not be a function type; consider a non-implicit parameter or a type class instead",
          )
        }
      }
    }
    tree
  }

  private def isFunctionLikeType(tpe: Type)(using Context): Boolean =
    defn.isFunctionType(tpe) || defn.isContextFunctionType(tpe) || tpe.isRef(defn.PartialFunctionClass)
}
