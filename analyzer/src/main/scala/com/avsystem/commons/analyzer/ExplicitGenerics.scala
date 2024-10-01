package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.{Contexts, Symbols}
import dotty.tools.dotc.transform.PickleQuotes

final class ExplicitGenerics extends AnalyzerRule("explicitGenerics") {

  import tpd.*

  //  lazy val explicitGenericsAnnotTpe = classType("com.avsystem.commons.annotation.explicitGenerics")

  //  def analyze(unit: CompilationUnit) = if (explicitGenericsAnnotTpe != NoType) {
  //    def requiresExplicitGenerics(sym: Symbol): Boolean =
  //      sym != NoSymbol && (sym :: sym.overrides).flatMap(_.annotations).exists(_.tree.tpe <:< explicitGenericsAnnotTpe)
  //
  //    def analyzeTree(tree: Tree): Unit = analyzer.macroExpandee(tree) match {
  //      case `tree` | EmptyTree =>
  //        tree match {
  //          case t@TypeApply(pre, args) if requiresExplicitGenerics(pre.symbol) =>
  //            val inferredTypeParams = args.forall {
  //              case tt: TypeTree => tt.original == null || tt.original == EmptyTree
  //              case _ => false
  //            }
  //            if (inferredTypeParams) {
  //              report(t.pos, s"${pre.symbol} requires that its type arguments are explicit (not inferred)")
  //            }
  //          case _ =>
  //        }
  //        tree.children.foreach(analyzeTree)
  //      case prevTree =>
  //        analyzeTree(prevTree)
  //    }
  //    analyzeTree(unit.body)
  //  }

  override def transformTypeApply(tree: TypeApply)(using Context): Tree = {
    val TypeApply(fun: Tree, args) = tree
    val explicitGenerics = Symbols.requiredClass("com.avsystem.commons.annotation.explicitGenerics")
    if fun.symbol.hasAnnotation(explicitGenerics) then args.foreach {
      case tt: TypeTree /*if tt.original == null || tt.original == EmptyTree */ =>
              report(s"${fun.symbol} requires that its type arguments are explicit (not inferred)", tree)
      case _ =>
    }
    tree
  }


}
