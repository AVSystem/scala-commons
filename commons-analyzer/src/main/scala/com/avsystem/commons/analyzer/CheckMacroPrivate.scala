package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

class CheckMacroPrivate[C <: Global with Singleton](g: C) extends AnalyzerRule(g, "macroPrivate") {

  import global._

  val macroPrivateAnnotTpe = classType("com.avsystem.commons.annotation.macroPrivate")

  def analyze(unit: CompilationUnit) = if (macroPrivateAnnotTpe != NoType) {
    def analyzeTree(tree: Tree): Unit = analyzer.macroExpandee(tree) match {
      case `tree` | EmptyTree =>
        tree match {
          case _: Ident | _: Select | _: SelectFromTypeTree | _: New
            if tree.symbol != null && tree.pos != NoPosition =>

            val sym = tree.symbol
            val macroPrivate = (sym :: sym.overrides).iterator
              .flatMap(_.annotations).exists(_.tree.tpe <:< macroPrivateAnnotTpe)
            if (macroPrivate) {
              report(tree.pos, s"$sym can only be used in macro-generated code")
            }
          case _ =>
        }
        tree.children.foreach(analyzeTree)
      case prevTree =>
        analyzeTree(prevTree)
    }
    analyzeTree(unit.body)
  }
}
