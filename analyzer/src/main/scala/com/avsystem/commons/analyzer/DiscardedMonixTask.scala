package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

class DiscardedMonixTask(g: Global) extends AnalyzerRule(g, "discardedMonixTask") {

  import global._

  lazy val monixTaskTpe: Type = classType("monix.eval.Task") match {
    case NoType => NoType
    case tpe => TypeRef(NoPrefix, tpe.typeSymbol, List(definitions.AnyTpe))
  }

  private def checkDiscardedTask(tree: Tree, discarded: Boolean): Unit = tree match {
    case tree if !discarded && tree.tpe != null && tree.tpe =:= definitions.UnitTpe =>
      checkDiscardedTask(tree, discarded = true)

    case Block(stats, expr) =>
      stats.foreach(checkDiscardedTask(_, discarded = true))
      checkDiscardedTask(expr, discarded)

    case Template(parents, self, body) =>
      parents.foreach(checkDiscardedTask(_, discarded = false))
      checkDiscardedTask(self, discarded = false)
      body.foreach(checkDiscardedTask(_, discarded = true))

    case If(_, thenp, elsep) =>
      checkDiscardedTask(thenp, discarded)
      checkDiscardedTask(elsep, discarded)

    case LabelDef(_, _, rhs) =>
      checkDiscardedTask(rhs, discarded = true)

    case Try(body, catches, finalizer) =>
      checkDiscardedTask(body, discarded)
      catches.foreach(checkDiscardedTask(_, discarded))
      checkDiscardedTask(finalizer, discarded = true)

    case CaseDef(_, _, body) =>
      checkDiscardedTask(body, discarded)

    case Match(_, cases) =>
      cases.foreach(checkDiscardedTask(_, discarded))

    case Annotated(_, arg) =>
      checkDiscardedTask(arg, discarded)

    case Typed(expr, _) =>
      checkDiscardedTask(expr, discarded)

    case Apply(TypeApply(Select(prefix, TermName("foreach")), List(_)), List(Function(_, body))) =>
      checkDiscardedTask(prefix, discarded = false)
      checkDiscardedTask(body, discarded)

    case _: Ident | _: Select | _: Apply | _: TypeApply
        if discarded && tree.tpe != null && tree.tpe <:< monixTaskTpe && !(tree.tpe <:< definitions.NullTpe) =>
      report(
        tree.pos,
        "discarded Monix Task - this is probably a mistake because the Task must be run for its side effects",
      )

    case tree =>
      tree.children.foreach(checkDiscardedTask(_, discarded = false))
  }

  def analyze(unit: CompilationUnit): Unit =
    if (monixTaskTpe != NoType) {
      checkDiscardedTask(unit.body, discarded = false)
    }
}
