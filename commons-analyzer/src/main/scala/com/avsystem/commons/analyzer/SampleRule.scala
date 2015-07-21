package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

class SampleRule[C <: Global with Singleton](g: C) extends AnalyzerRule(g) {

  import global._

  def analyze(unit: CompilationUnit): Unit = {
    //reporter.warning(unit.body.pos, "Sample warning")
  }
}
