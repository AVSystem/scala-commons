package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

class ShowAst[C <: Global with Singleton](g: C) extends AnalyzerRule(g, "showAst", Level.Error) {

  import global._

  lazy val showAstAnnotType: Type = classType("com.avsystem.commons.annotation.showAst")

  def analyze(unit: CompilationUnit) = if (showAstAnnotType != NoType) {
    def analyzeTree(tree: Tree): Unit = analyzer.macroExpandee(tree) match {
      case `tree` | EmptyTree =>
        tree match {
          case Annotated(annot, arg) if annot.tpe <:< showAstAnnotType =>
            report(arg.pos, showCode(arg))
          case Typed(expr, tpt) if tpt.tpe.annotations.exists(_.tpe <:< showAstAnnotType) =>
            report(expr.pos, showCode(expr))
          case _: MemberDef if tree.symbol.annotations.exists(_.tpe <:< showAstAnnotType) =>
            report(tree.pos, showCode(tree))
          case _ =>
        }
        tree.children.foreach(analyzeTree)
      case prevTree =>
        analyzeTree(prevTree)
    }
    analyzeTree(unit.body)
  }
}
