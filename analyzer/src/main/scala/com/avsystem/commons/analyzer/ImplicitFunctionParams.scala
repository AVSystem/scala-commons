package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

class ImplicitFunctionParams(g: Global) extends AnalyzerRule(g, "implicitFunctionParams", Level.Warn) {

  import global.*

  def analyze(unit: CompilationUnit): Unit = unit.body.foreach {
    case dd: DefDef =>
      dd.vparamss.foreach { paramList =>
        if (paramList.nonEmpty && paramList.head.mods.hasFlag(Flag.IMPLICIT)) {
          paramList.foreach { param =>
            val paramTpe = param.tpt.tpe
            if (paramTpe != null && (definitions.isFunctionType(paramTpe) || definitions.isPartialFunctionType(paramTpe))) {
              report(param.pos, "Implicit parameters should not have any function type")
            }
          }
        }
      }
    case _ =>
  }
}
