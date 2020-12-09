package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

class CheckBincompat(g: Global) extends AnalyzerRule(g, "bincompat") {

  import global._

  private lazy val bincompatAnnotType = classType("com.avsystem.commons.annotation.bincompat")

  def analyze(unit: CompilationUnit): Unit =
    unit.body.foreach(analyzeTree {
      case tree@(_: Ident | _: Select | _: New) if tree.symbol != null &&
        tree.symbol.annotations.exists(_.tree.tpe <:< bincompatAnnotType) =>
        report(tree.pos, "Symbols annotated as @bincompat exist only for binary compatibility " +
          "and should not be used directly")
    })
}
