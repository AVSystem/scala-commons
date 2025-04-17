package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global
import scala.util.control.NonFatal

class CatchThrowable(g: Global) extends AnalyzerRule(g, "catchThrowable", Level.Warn) {

  import global._

  private lazy val throwableTpe = typeOf[Throwable]
  private lazy val nonFatalSym = typeOf[NonFatal.type].termSymbol

  private def isNonFatalPattern(tree: Tree): Boolean = tree match {
    case UnApply(Apply(Select(qualifier, TermName("unapply")), _), _) if qualifier.symbol == nonFatalSym => true
    case _ => false
  }

  def analyze(unit: CompilationUnit): Unit =
    unit.body.foreach {
      case t: Try =>
        t.catches.foreach { case cas@CaseDef(pat, _, _) =>
          if (pat.tpe != null && pat.tpe =:= throwableTpe && !isNonFatalPattern(pat)) {
            report(cas.pos, "Catching Throwable is discouraged, catch specific exceptions instead")
          }
        }
      case _ =>
    }
}
