package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

class CatchThrowable(g: Global) extends AnalyzerRule(g, "catchThrowable", Level.Warn) {

  import global.*

  private lazy val throwableTpe = typeOf[Throwable]

  private def isCustomExtractor(tree: Tree): Boolean = tree match {
    case UnApply(Apply(Select(_, TermName("unapply")), _), _) => true
    case _ => false
  }

  private def checkTree(pat: Tree): Unit = if (pat.tpe != null && pat.tpe =:= throwableTpe && !isCustomExtractor(pat)) {
    report(pat.pos, "Catching Throwable is discouraged, catch specific exceptions instead")
  }

  def analyze(unit: CompilationUnit): Unit =
    unit.body.foreach {
      case t: Try =>
        t.catches.foreach {
          case CaseDef(Alternative(trees), _, _) => trees.foreach(checkTree)
          case CaseDef(Bind(_, Alternative(trees)), _, _) => trees.foreach(checkTree)
          case CaseDef(pat, _, _) => checkTree(pat)
        }
      case _ =>
    }
}
