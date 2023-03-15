package com.avsystem.commons
package analyzer

import scala.annotation.tailrec
import scala.tools.nsc.Global

class BasePackage(g: Global) extends AnalyzerRule(g, "basePackage") {

  import global._

  object ImportsOnlyOrEmptyList {
    @tailrec def unapply(stats: List[Tree]): Option[Tree] = stats match {
      case Nil => None
      case List(stat) => Some(stat)
      case head :: next => head match {
        case Import(_, _) => unapply(next)
        case _ => None
      }
    }
  }

  def analyze(unit: CompilationUnit): Unit = if (argument != null) {
    val requiredBasePackage = argument

    @tailrec def validate(tree: Tree): Unit = tree match {
      case PackageDef(pid, _) if pid.symbol.hasPackageFlag && pid.symbol.fullName == requiredBasePackage =>
      case PackageDef(_, ImportsOnlyOrEmptyList(stat)) => validate(stat)
      case t => report(t.pos, s"`$requiredBasePackage` must be one of the base packages in this file")
    }

    validate(unit.body)
  }
}
