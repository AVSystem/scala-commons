package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

class ExplicitGenerics(g: Global) extends AnalyzerRule(g, "explicitGenerics") {

  import global._

  lazy val explicitGenericsAnnotTpe = classType("com.avsystem.commons.annotation.explicitGenerics")


  private def fail(pos: Position, symbol: Symbol): Unit =
    report(pos, s"$symbol requires that its type arguments are explicit (not inferred)")

  def analyze(unit: CompilationUnit): Unit = if (explicitGenericsAnnotTpe != NoType) {
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
              fail(t.pos, pre.symbol)
            }
          case n@New(tpt) if requiresExplicitGenerics(tpt.tpe.typeSymbol) =>
            val explicitTypeArgsProvided = tpt match {
              case tt: TypeTree => tt.original match {
                case AppliedTypeTree(_, args) if args.nonEmpty => true
                case _ => false
              }
              case _ => false
            }
            if (!explicitTypeArgsProvided) {
              fail(n.pos, tpt.tpe.typeSymbol)
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
