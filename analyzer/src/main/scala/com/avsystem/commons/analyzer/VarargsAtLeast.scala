package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.core.Constants.Constant

class VarargsAtLeast extends AnalyzerRule {
  val name: String = "varargsAtLeast"

  override def transformApply(tree: tpd.Apply)(using ctx: Context): tpd.Tree = {
    val fun = tree.fun
    val args = tree.args

    if (fun.symbol != NoSymbol) {
      val paramInfoss = fun.symbol.info.paramInfoss
      if (paramInfoss.nonEmpty) {
        val params = paramInfoss.head
        if (params.nonEmpty && params.last.isRepeatedParam) {
          // In Scala 3, varargs are always passed as a single Typed(SeqLiteral(...), tpt) node.
          // Explicit splices (seq*) are Typed(expr, tpt) where expr is NOT a SeqLiteral.
          // We need to look inside the Typed to distinguish and count elements.
          args.lastOption match {
            case Some(tpd.Typed(seqLit: tpd.SeqLiteral, _)) =>
              // Compiler-generated vararg pack -- count the elements
              checkAtLeast(tree, fun, params, seqLit.elems.size)
            case Some(_: tpd.Typed) =>
              // Explicit splice (e.g., list*) -- skip the check
              ()
            case _ =>
              // No Typed wrapper (shouldn't happen for varargs, but handle gracefully)
              ()
          }
        }
      }
    }
    tree
  }

  private def checkAtLeast(
    tree: tpd.Apply,
    fun: tpd.Tree,
    params: List[?],
    actualVarargCount: Int,
  )(using ctx: Context,
  ): Unit = {
    val atLeastAnnotClass = Symbols.getClassIfDefined("com.avsystem.commons.annotation.atLeast")

    if (atLeastAnnotClass != NoSymbol) {
      val methodParams = fun.symbol.paramSymss.flatten
      val lastParamSym = methodParams.lastOption

      lastParamSym.foreach { paramSym =>
        paramSym.annotations.foreach { annot =>
          if (annot.symbol == atLeastAnnotClass) {
            val required = annot.arguments match {
              case List(tpd.Literal(Constant(n: Int))) => n
              case _ => 0
            }

            if (actualVarargCount < required) {
              report(
                tree,
                s"This method requires at least $required arguments for its repeated parameter, $actualVarargCount passed.",
              )
            }
          }
        }
      }
    }
  }
}
