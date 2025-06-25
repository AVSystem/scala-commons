package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

final class NothingAsFunctionArgument(g: Global) extends AnalyzerRule(g, "nothingAsFunctionArgument") {

  import global.*

  def analyze(unit: CompilationUnit): Unit = unit.body.foreach(analyzeTree {
    case Apply(f: Tree, args: List[Tree]) =>
      args.zip(f.tpe.params).foreach {
        case (arg, param) if definitions.isFunctionType(param.tpe) && arg.tpe <:< definitions.NothingTpe =>
          report(arg.pos,
            s"""
               |A value of type `Nothing` was passed where a function is expected.
               |If you intended to throw an exception, wrap it in a function literal (e.g. `_ => throw ex` instead of `throw ex`).
               |If you are using a mocking framework, provide a mock function with the correct type (e.g. `any[${show(param.tpe)}]`).
               |""".stripMargin
          )
        case (_, _) =>
      }
  })
}
