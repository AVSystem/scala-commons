package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Symbols.{NoSymbol, Symbol}
import dotty.tools.dotc.core.Types.Type

class DiscardedMonixTask extends AnalyzerRule {
  val name: String = "discardedMonixTask"

  private def monixTaskClass(using Context): Symbol =
    Symbols.getClassIfDefined("monix.eval.Task")

  override def transformUnit(tree: tpd.Tree)(using ctx: Context): tpd.Tree = {
    val taskCls = monixTaskClass
    if (taskCls != NoSymbol) {
      checkDiscardedTask(tree, discarded = false)
    }
    tree
  }

  private def checkDiscardedTask(tree: tpd.Tree, discarded: Boolean)(using ctx: Context): Unit = {
    tree match {
      case tpd.Block(stats, expr) =>
        stats.foreach(checkDiscardedTask(_, discarded = true))
        checkDiscardedTask(expr, discarded)

      case templ: tpd.Template =>
        templ.body.foreach(checkDiscardedTask(_, discarded = true))

      case tpd.If(_, thenp, elsep) =>
        checkDiscardedTask(thenp, discarded)
        checkDiscardedTask(elsep, discarded)

      case tpd.WhileDo(_, body) =>
        checkDiscardedTask(body, discarded = true)

      case tpd.Try(body, catches, finalizer) =>
        checkDiscardedTask(body, discarded)
        catches.foreach(checkDiscardedTask(_, discarded))
        if (!finalizer.isEmpty) checkDiscardedTask(finalizer, discarded = true)

      case tpd.CaseDef(_, _, body) =>
        checkDiscardedTask(body, discarded)

      case tpd.Match(_, cases) =>
        cases.foreach(checkDiscardedTask(_, discarded))

      case tree: tpd.DefDef =>
        if (!tree.rhs.isEmpty) checkDiscardedTask(tree.rhs, discarded = false)

      case tree: tpd.ValDef =>
        if (!tree.rhs.isEmpty) checkDiscardedTask(tree.rhs, discarded = false)

      case tree: tpd.TypeDef =>
        checkDiscardedTask(tree.rhs, discarded = false)

      case tpd.PackageDef(_, stats) =>
        stats.foreach(checkDiscardedTask(_, discarded = false))

      // Leaf nodes: detect discarded Task values
      case tree @ (_: tpd.Ident | _: tpd.Select | _: tpd.Apply | _: tpd.TypeApply)
          if discarded && isTaskType(tree.tpe) =>
        report(tree, "discarded monix.eval.Task value - this Task will not execute its side effects")

      case _ =>
        ()
    }
  }

  private def isTaskType(tpe: Type)(using ctx: Context): Boolean = {
    val taskCls = monixTaskClass
    tpe != null && taskCls != NoSymbol && tpe.widenDealias.classSymbol.derivesFrom(taskCls)
  }
}
