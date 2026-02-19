package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol

class NothingAsFunctionArgument extends AnalyzerRule {
  val name: String = "nothingAsFunctionArgument"

  override def transformApply(tree: tpd.Apply)(using ctx: Context): tpd.Tree = {
    val fun = tree.fun
    val args = tree.args

    if (fun.symbol != NoSymbol) {
      val paramInfoss = fun.symbol.info.paramInfoss
      if (paramInfoss.nonEmpty) {
        val params = paramInfoss.head
        args.zip(params).foreach { case (arg, paramTpe) =>
          val isFunctionTpe = ctx.definitions.isFunctionType(paramTpe)
          val isNothingTpe = arg.tpe <:< ctx.definitions.NothingType
          if (isFunctionTpe && isNothingTpe) {
            report(
              arg,
              s"""|A value of type `Nothing` was passed where a function is expected.
                  |If you intended to throw an exception, wrap it in a function literal (e.g. `_ => throw ex` instead of `throw ex`).
                  |If you are using a mocking framework, provide a mock function with the correct type (e.g. `any[${paramTpe.show}]`).
                  |""".stripMargin,
            )
          }
        }
      }
    }
    tree
  }
}
