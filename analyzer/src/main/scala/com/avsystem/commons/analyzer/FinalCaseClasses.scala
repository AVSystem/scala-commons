package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

class FinalCaseClasses(g: Global) extends AnalyzerRule(g, "finalCaseClasses", Level.Warn) {

  import global.*

  def analyze(unit: CompilationUnit): Unit = unit.body.foreach {
    case cd: ClassDef if !cd.mods.hasFlag(Flag.FINAL) && cd.mods.hasFlag(Flag.CASE) =>
      report(cd.pos, "Case classes should be marked as final")
    case _ =>
  }
}