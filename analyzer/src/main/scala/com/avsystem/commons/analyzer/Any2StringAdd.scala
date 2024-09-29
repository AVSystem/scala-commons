//package com.avsystem.commons
//package analyzer
//
//import dotty.tools.dotc.ast.tpd
//import dotty.tools.dotc.core.Contexts.Context
//import dotty.tools.dotc.core.Symbols
//import dotty.tools.dotc.core.Symbols.*
//
//final class Any2StringAdd extends AnalyzerRule("any2stringadd", Level.Off):
//  import tpd.*
//
//  private lazy val any2stringaddSym: Context ?=> Symbol =
//    Symbols.requiredClass("scala.Predef").classDenot.requiredMethod("any2stringadd")
//
//  override protected def analyzeTree(using Context): PartialFunction[Tree, Unit] = {
//    case tree if tree.symbol == any2stringaddSym =>
//      report(
//        "concatenating arbitrary values with strings is disabled, " +
//          "use explicit toString or string interpolation",
//        tree.symbol
//      )(using tree.sourcePos)
//  }
//
//end Any2StringAdd
