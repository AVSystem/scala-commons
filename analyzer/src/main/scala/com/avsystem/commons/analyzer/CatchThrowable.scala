package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

class CatchThrowable(g: Global) extends AnalyzerRule(g, "catchThrowable", Level.Warn) {

  import global._

  private lazy val throwableTpe = typeOf[Throwable]

  def analyze(unit: CompilationUnit): Unit =
    unit.body.foreach {
      case t: Try =>
        t.catches.foreach { case cas@CaseDef(pat, _, _) =>
          if (pat.tpe != null && pat.tpe =:= throwableTpe) {
            report(cas.pos, "Catching Throwable is discouraged, catch specific exceptions instead")
          }
        }
      case _ =>
    }
}
