package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

final class ThrownExceptionNotInFunction(g: Global) extends AnalyzerRule(g, "thrownExceptionNotInFunction") {

  import global.*

  def analyze(unit: CompilationUnit): Unit = unit.body.foreach(analyzeTree {
    case t@Apply(f: TypeApply, List(Throw(_))) if definitions.isFunctionType(f.tpe.params.head.tpe) =>
      report(t.pos, "exception thrown in place of function definition")
  })
}
