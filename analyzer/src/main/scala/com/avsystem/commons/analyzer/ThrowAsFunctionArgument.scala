package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

final class ThrowAsFunctionArgument(g: Global) extends AnalyzerRule(g, "throwAsFunctionArgument") {

  import global.*

  def analyze(unit: CompilationUnit): Unit = unit.body.foreach(analyzeTree {
    case Apply(f: Tree, args: List[Tree]) =>
      args.zip(f.tpe.params).foreach {
        case (arg, param) if definitions.isFunctionType(param.tpe) && arg.tpe <:< definitions.NothingTpe =>
          report(arg.pos, "Exception thrown in place of function definition. Wrap the exception in a function literal instead (e.g. `_ => throw ex` instead of `throw ex`)")
        case (_, _) =>
      }
  })
}
