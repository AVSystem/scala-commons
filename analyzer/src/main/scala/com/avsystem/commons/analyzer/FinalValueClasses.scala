package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

class FinalValueClasses(g: Global) extends AnalyzerRule(g, "finalValueClasses", Level.Warn) {

  import global.*

  private lazy val anyValTpe = typeOf[AnyVal]

  def analyze(unit: CompilationUnit): Unit = unit.body.foreach {
    case cd: ClassDef if !cd.mods.hasFlag(Flag.FINAL) =>
      val tpe = cd.symbol.typeSignature

      if (tpe.baseClasses.contains(anyValTpe.typeSymbol)) {
        report(cd.pos, "Value classes should be marked as final")
      }
    case _ =>
  }
}
