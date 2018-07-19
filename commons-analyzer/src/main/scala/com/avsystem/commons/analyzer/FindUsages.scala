package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

class FindUsages[C <: Global with Singleton](g: C) extends AnalyzerRule(g, "findUsages") {

  import global._

  lazy val rejectedSymbols: Set[String] =
    if (argument == null) Set.empty else argument.split(";").toSet

  override def analyze(unit: CompilationUnit): Unit = if (rejectedSymbols.nonEmpty) {
    unit.body.foreach { tree =>
      if (tree.symbol != null && rejectedSymbols.contains(tree.symbol.fullName)) {
        report(tree.pos, s"found usage of ${tree.symbol.fullName}")
      }
    }
  }
}
