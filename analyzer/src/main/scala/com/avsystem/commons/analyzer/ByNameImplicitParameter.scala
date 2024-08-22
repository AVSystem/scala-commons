package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

final class ByNameImplicitParameter(g: Global) extends AnalyzerRule(g, "byNameImplicitParameter") {

  import global.*

  private def isByNameImplicit(x: ValDef): Boolean =
    x.symbol.isImplicit && x.symbol.isByNameParam && !x.symbol.isSynthetic

  def analyze(unit: CompilationUnit): Unit = unit.body.foreach {
    case tree: DefDef if !tree.symbol.isSynthetic && tree.vparamss.flatten.exists(isByNameImplicit) =>
      report(tree.pos, "Implicit by-name parameters are disabled")
    case _ =>
  }

}
