package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

class VarargsAtLeast(g: Global) extends AnalyzerRule(g, "varargsAtLeast") {

  import global._

  lazy val atLeastAnnotTpe = classType("com.avsystem.commons.annotation.atLeast")

  def analyze(unit: CompilationUnit): Unit = if (atLeastAnnotTpe != NoType) {
    def isVarargParam(tree: Tree) = tree match {
      case Typed(_, Ident(typeNames.WILDCARD_STAR)) => true
      case _ => false
    }

    unit.body.foreach(analyzeTree {
      case t@Apply(fun, args)
        if fun.tpe.params.lastOption.map(_.tpe.typeSymbol).contains(definitions.RepeatedParamClass) &&
          !args.lastOption.exists(isVarargParam) =>

        val required =
          fun.tpe.params.last.annotations.find(_.tree.tpe <:< atLeastAnnotTpe).map(_.tree.children.tail).collect {
            case List(Literal(Constant(n: Int))) => n
          }.getOrElse(0)

        val actual = args.size - fun.tpe.params.size + 1

        if (actual < required) {
          report(t.pos,
            s"This method requires at least $required arguments for its repeated parameter, $actual passed.")
        }
    })
  }
}
