package com.avsystem.commons
package analyzer

import com.avsystem.commons.analyzer.Level.Info

import scala.tools.nsc.Global

class FinalCaseClasses(g: Global) extends AnalyzerRule(g, "finalCaseClasses", Level.Warn) {

  import global.*

  def analyze(unit: CompilationUnit): Unit = unit.body.foreach {
    case cd: ClassDef if !cd.mods.hasFlag(Flag.FINAL | Flag.SEALED) && cd.mods.hasFlag(Flag.CASE) =>
      // Skip case classes defined inside traits (SI-4440)
      val isInner = cd.symbol.isStatic

      if (isInner) {
        report(cd.pos, "Case classes should be marked as final")
      } else {
        report(
          cd.pos,
          "Case classes should be marked as final. Due to the SI-4440 bug, it cannot be done here. Consider moving the case class to the companion object",
          level = Info,
        )
      }
    case _ =>
  }
}
