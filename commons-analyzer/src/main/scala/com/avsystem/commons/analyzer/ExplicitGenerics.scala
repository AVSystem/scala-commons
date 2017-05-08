package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

class ExplicitGenerics[C <: Global with Singleton](g: C) extends AnalyzerRule(g, "explicitGenerics") {

  import global._

  val explicitGenericsAnnotTpe = classType("com.avsystem.commons.annotation.explicitGenerics")

  def analyze(unit: CompilationUnit) = if (explicitGenericsAnnotTpe != NoType) {
    def requiresExplicitGenerics(sym: Symbol): Boolean =
      sym != NoSymbol && (sym :: sym.overrides).flatMap(_.annotations).exists(_.tree.tpe <:< explicitGenericsAnnotTpe)

    def analyzeTree(tree: Tree): Unit = analyzer.macroExpandee(tree) match {
      case `tree` | EmptyTree =>
        tree match {
          case t@TypeApply(pre, args) if requiresExplicitGenerics(pre.symbol) =>
            val inferredTypeParams = args.forall {
              case tt: TypeTree => tt.original == null || tt.original == EmptyTree
              case _ => false
            }
            if (inferredTypeParams) {
              report(t.pos, s"${pre.symbol} requires that its type arguments are explicit (not inferred)")
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
