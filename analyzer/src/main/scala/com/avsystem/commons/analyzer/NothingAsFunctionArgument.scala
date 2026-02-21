package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.{defn, NoSymbol}

class NothingAsFunctionArgument extends AnalyzerRule {
  val name: String = "nothingAsFunctionArgument"

  override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree = {
    if (tree.fun.symbol != NoSymbol) {
      val paramInfoss = tree.fun.symbol.info.paramInfoss
      if (paramInfoss.nonEmpty) {
        val params = paramInfoss.head
        tree.args.zip(params).foreach { (arg, paramTpe) =>
          if (defn.isFunctionType(paramTpe) && arg.tpe <:< defn.NothingType) {
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
