//package com.avsystem.commons
//package analyzer
//
//import scala.annotation.tailrec
//import scala.tools.nsc.Global
//
//class BasePackage(g: Global) extends AnalyzerRule(g, "basePackage") {
//
//  import global._
//
//  object SkipImports {
//    @tailrec def unapply(stats: List[Tree]): Some[List[Tree]] = stats match {
//      case Import(_, _) :: tail => unapply(tail)
//      case stats => Some(stats)
//    }
//  }
//
//  def analyze(unit: CompilationUnit): Unit = if (argument != null) {
//    val requiredBasePackage = argument
//
//    @tailrec def validate(tree: Tree): Unit = tree match {
//      case PackageDef(pid, _) if pid.symbol.hasPackageFlag && pid.symbol.fullName == requiredBasePackage =>
//      case PackageDef(_, SkipImports(List(stat))) => validate(stat)
//      case t => report(t.pos, s"`$requiredBasePackage` must be one of the base packages in this file")
//    }
//
//    validate(unit.body)
//  }
//}
