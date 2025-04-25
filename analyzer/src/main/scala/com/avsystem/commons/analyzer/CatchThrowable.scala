package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global
import scala.util.control.NonFatal

class CatchThrowable(g: Global) extends AnalyzerRule(g, "catchThrowable", Level.Warn) {

  import global._

  private lazy val throwableTpe = typeOf[Throwable]

  private lazy val nonFatalSymbols = {
    val nonFatal = typeOf[NonFatal.type].termSymbol
    val nonFatalAlias = classType("com.avsystem.commons.CommonAliases").member(TermName("NonFatal"))

    Set(nonFatal, nonFatalAlias)
  }

  private def isNonFatalPattern(tree: Tree): Boolean = tree match {
    case UnApply(Apply(Select(qualifier, TermName("unapply")), _), _) if nonFatalSymbols contains qualifier.symbol => true
    case _ => false
  }

  private def checkTree(pat: Tree): Unit = if (pat.tpe != null && pat.tpe =:= throwableTpe && !isNonFatalPattern(pat)) {
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
