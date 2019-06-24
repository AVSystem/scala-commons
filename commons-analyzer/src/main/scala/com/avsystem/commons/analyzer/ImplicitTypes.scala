package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

class ImplicitTypes(g: Global) extends AnalyzerRule(g, "implicitTypes") {

  import global._

  def analyze(unit: CompilationUnit): Unit = unit.body.foreach {
    case t@ValOrDefDef(mods, _, tpt@TypeTree(), _) if tpt.original == null && mods.isImplicit && !mods.isSynthetic =>
      report(t.pos, s"Implicit definitions must have type annotated explicitly")
    case _ =>
  }
}
