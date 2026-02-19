package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

import scala.annotation.tailrec

class BasePackage extends AnalyzerRule {
  val name: String = "basePackage"

  override def transformUnit(tree: tpd.Tree)(using Context): tpd.Tree = {
    argument.foreach { requiredBase =>
      validate(tree, requiredBase)
    }
    tree
  }

  @tailrec
  private def validate(tree: tpd.Tree, required: String)(using Context): Unit = tree match {
    case pkg: tpd.PackageDef if pkg.pid.symbol.fullName.toString == required =>
      // Found the required base package -- validation passes
      ()
    case pkg: tpd.PackageDef =>
      // Skip imports, recurse into the single remaining stat (if exactly one)
      val nonImports = pkg.stats.filterNot(_.isInstanceOf[tpd.Import])
      nonImports match {
        case stat :: Nil => validate(stat, required)
        case _ =>
          report(tree, s"`$required` must be one of the base packages in this file")
      }
    case _ =>
      report(tree, s"`$required` must be one of the base packages in this file")
  }
}
