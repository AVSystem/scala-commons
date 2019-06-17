package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

class Any2StringAdd[C <: Global with Singleton](g: C) extends AnalyzerRule(g, "any2stringadd", Level.Off) {

  import global._

  private lazy val any2stringaddSym =
    typeOf[Predef.type].member(TermName("any2stringadd")).alternatives.find(_.isMethod).get

  def analyze(unit: CompilationUnit): Unit = {
    unit.body.foreach(analyzeTree {
      case t if t.symbol == any2stringaddSym =>
        report(t.pos, "concatenating arbitrary values with strings is disabled, " +
          "use explicit toString or string interpolation")
    })
  }
}
