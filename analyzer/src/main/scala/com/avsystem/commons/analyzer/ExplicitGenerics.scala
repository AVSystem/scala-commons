package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.{Contexts, Symbols}

final class ExplicitGenerics extends AnalyzerRule("explicitGenerics"):
  import ExplicitGenerics.*

  override def transformTypeApply(tree: TypeApply)(using Context): Tree = tree.tap {
    case TypeApply(fun: Tree, args)
        if fun.symbol.hasAnnotation(explicitGenericsSymbol) && args.containsInferredTypeParameters =>
      report(s"${fun.symbol} requires that its type arguments are explicit (not inferred)", tree)
    case _ =>
  }

  override def transformInlined(tree: Inlined)(using Context): Tree = tree.tap {
    case Inlined(Apply(typeApply: TypeApply, _), _, _) => transformTypeApply(typeApply)
    case _                                             =>
  }

end ExplicitGenerics

private object ExplicitGenerics:

  private final val explicitGenericsSymbol =
    (ctx: Context) ?=> Symbols.requiredClass("com.avsystem.commons.annotation.explicitGenerics")

  extension (args: List[Tree])
    private def containsInferredTypeParameters: Boolean = args.exists(_.isInstanceOf[TypeTree])

end ExplicitGenerics
