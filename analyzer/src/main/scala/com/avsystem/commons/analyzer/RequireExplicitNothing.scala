package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

final class RequireExplicitNothing(g: Global) extends AnalyzerRule(g, "requireExplicitNothing") {

  import global.*

  private def wasInferred(t: TypeTree) = t.original == null

  def analyze(unit: CompilationUnit): Unit = unit.body.foreach(analyzeTree {
    case tree@TypeTree() if tree.tpe <:< definitions.NothingTpe && wasInferred(tree) =>
      tree.tpe match {
        // Ignore existential types, they supposedly contain "any"
        case ExistentialType(_, _) =>

        case _ => report(tree.pos, "Inferred type: Nothing")
      }
  })
}
