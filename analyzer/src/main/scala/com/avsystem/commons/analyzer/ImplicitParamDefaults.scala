package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

class ImplicitParamDefaults(g: Global) extends AnalyzerRule(g, "implicitParamDefaults", Level.Warn) {

  import global.*

  def analyze(unit: CompilationUnit): Unit = unit.body.foreach {
    case dd: DefDef =>
      dd.vparamss.foreach { paramList =>
        if (paramList.nonEmpty && paramList.head.mods.hasFlag(Flag.IMPLICIT)) {
          paramList.foreach { param =>
            if (param.rhs != EmptyTree) {
              report(param.pos, "Implicit parameters should not have default values")
            }
          }
        }
      }
    case _ =>
  }
}
