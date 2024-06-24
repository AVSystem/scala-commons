package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

final class ThrownExceptionNotInFunction(g: Global) extends AnalyzerRule(g, "thrownExceptionNotInFunction") {

  import global.*

  def analyze(unit: CompilationUnit): Unit = unit.body.foreach(analyzeTree {
    case Apply(f: Tree, args: List[Tree]) =>
      args.zip(f.tpe.params).foreach {
        case (arg, param) if definitions.isFunctionType(param.tpe) && arg.tpe <:< definitions.NothingTpe =>
          report(arg.pos, "exception thrown in place of function definition")
        case (_, _) =>
      }
  })
}
