package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Symbols.{NoSymbol, Symbol}

class DiscardedMonixTask(using Context) extends AnalyzerRule {
  val name: String = "discardedMonixTask"

  private val monixTaskClass: Symbol = Symbols.getClassIfDefined("monix.eval.Task")
  override def transformBlock(tree: tpd.Block)(using Context): tpd.Tree = {
    tree.stats.foreach(reportIfTask)
    tree
  }
  override def transformTemplate(tree: tpd.Template)(using Context): tpd.Tree = {
    tree.body.foreach {
      case _: tpd.DefTree => ()
      case stat => reportIfTask(stat)
    }
    tree
  }
  override def transformWhileDo(tree: tpd.WhileDo)(using Context): tpd.Tree = {
    reportIfTask(tree.body)
    tree
  }
  override def transformTry(tree: tpd.Try)(using Context): tpd.Tree = {
    if (!tree.finalizer.isEmpty) {
      reportIfTask(tree.finalizer)
    }
    tree
  }
  private def reportIfTask(tree: tpd.Tree)(using Context): Unit =
    if (monixTaskClass != NoSymbol && tree.tpe.widenDealias.classSymbol.derivesFrom(monixTaskClass)) {
      report(tree, "discarded monix.eval.Task value - this Task will not execute its side effects")
    }

}
