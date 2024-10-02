package com.avsystem.commons
package analyzer

import com.avsystem.commons.analyzer.{AnalyzerRule, ExplicitGenerics}
import com.avsystem.commons.analyzer.ExplicitGenerics.explicitGenericsSymbol
import com.avsystem.commons.analyzer.VarargsAtLeast.atLeastSymbol
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols
//package com.avsystem.commons
//package analyzer
//
//final class VarargsAtLeast extends AnalyzerRule("varargsAtLeast"):
//
//  lazy val atLeastAnnotTpe = classType("com.avsystem.commons.annotation.atLeast")
//
//  def analyze(unit: CompilationUnit): Unit = if atLeastAnnotTpe != NoType then
//    def isVarargParam(tree: Tree) = tree match
//      case Typed(_, Ident(typeNames.WILDCARD_STAR)) => true
//      case _                                        => false
//
//    unit.body.foreach(analyzeTree {
//      case t @ Apply(fun, args)
//          if fun.tpe != null && fun.tpe.params.lastOption
//            .map(_.tpe.typeSymbol)
//            .contains(definitions.RepeatedParamClass) &&
//            !args.lastOption.exists(isVarargParam) =>
//        val required =
//          fun.tpe.params.last.annotations
//            .find(_.tree.tpe <:< atLeastAnnotTpe)
//            .map(_.tree.children.tail)
//            .collect { case List(Literal(Constant(n: Int))) =>
//              n
//            }
//            .getOrElse(0)
//
//        val actual = args.size - fun.tpe.params.size + 1
//
//        if actual < required then
//          report(
//            t.pos,
//            s"This method requires at least $required arguments for its repeated parameter, $actual passed."
//          )
//
//        end if
//    })
//
//end VarargsAtLeast

final class VarargsAtLeast extends AnalyzerRule("varargsAtLeast"):

  override def transformApply(tree: Apply)(using Context): Tree = tree.tap {
    case Apply(fun: Tree, argss: List[Tree]) =>
      val paramss = fun.symbol.denot.paramSymss

      paramss.zip(argss).foreach { case (params, args) =>
//        params.zip(args).foreach { case (param, arg) =>
//          println("dupa")
//        }

//        param.getAnnotation(atLeastSymbol).flatMap(_.argumentConstant(0)).map(_.intValue).foreach { required =>

        println("dupa")
      }

//        if param.isRepeatedParam && !args.lastOption.exists {
//          case Typed(_, Ident(tpnme.WILDCARD_STAR)) => true
//          case _ => false
//        } then
//          val required = param.annotations
//            .find(_.tree.tpe <:< atLeastAnnotTpe)
//            .map(_.tree.children.tail)
//            .collect { case List(Literal(Constant(n: Int))) =>
//              n
//            }
//            .getOrElse(0)
//
//          val actual = args.size - params.size + 1
//
//          if actual < required then
//            report(
//              s"This method requires at least $required arguments for its repeated parameter, $actual passed.",
//              tree
//            )
    case _ =>
  }

end VarargsAtLeast

object VarargsAtLeast:

  private final val atLeastSymbol =
    (ctx: Context) ?=> Symbols.requiredClass("com.avsystem.commons.annotation.atLeast")

end VarargsAtLeast
