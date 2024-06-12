package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

final class ThrownExceptionNotInMonixScope(g: Global) extends AnalyzerRule(g, "thrownExceptionNotInMonixScope") {

  import global.*

  lazy val monixTaskTpe: Type = classType("monix.eval.Task") match {
    case NoType => NoType
    case tpe => TypeRef(NoPrefix, tpe.typeSymbol, List(definitions.AnyTpe))
  }
  private def checkDiscardedNothing(tree: Tree, discarded: Boolean): Unit = tree match {
    case tree if !discarded && tree.tpe != null && tree.tpe =:= definitions.NothingTpe =>
      checkDiscardedNothing(tree, discarded = true)

    case Block(stats: List[Tree], expr: Tree) =>
      stats.foreach(checkDiscardedNothing(_, discarded = true))
      checkDiscardedNothing(expr, discarded)

    case Template(parents: List[Tree], self: ValDef, body: List[Tree]) =>
      parents.foreach(checkDiscardedNothing(_, discarded = false))
      checkDiscardedNothing(self, discarded = false)
      body.foreach(checkDiscardedNothing(_, discarded = true))

    case If(_: Tree, thenp: Tree, elsep: Tree) =>
      checkDiscardedNothing(thenp, discarded)
      checkDiscardedNothing(elsep, discarded)

    case LabelDef(_: TermName, _: List[Ident], rhs: Tree) =>
      checkDiscardedNothing(rhs, discarded = true)

    case Try(block: Tree, catches: List[CaseDef], finalizer: Tree) =>
      checkDiscardedNothing(block, discarded)
      catches.foreach(checkDiscardedNothing(_, discarded))
      checkDiscardedNothing(finalizer, discarded = true)

    case CaseDef(_: Tree, _: Tree, body: Tree) =>
      checkDiscardedNothing(body, discarded)

    case Match(_: Tree, cases: List[CaseDef]) =>
      cases.foreach(checkDiscardedNothing(_, discarded))

    case Annotated(_: Tree, arg: Tree) =>
      checkDiscardedNothing(arg, discarded)

    case Typed(expr: Tree, _: Tree) =>
      checkDiscardedNothing(expr, discarded)

    case Apply(TypeApply(Select(prefix: Tree, TermName("map")), List(_)), List(Throw(_))) if prefix.tpe <:< monixTaskTpe =>
      report(tree.pos, "exception thrown not in Monix Task scope ")

    case tree =>
      tree.children.foreach(checkDiscardedNothing(_, discarded = false))
  }

  def analyze(unit: CompilationUnit): Unit = checkDiscardedNothing(unit.body, discarded = false)
}
