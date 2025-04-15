package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

class ImplicitValueClasses(g: Global) extends AnalyzerRule(g, "implicitValueClasses", Level.Warn) {

  import global.*

  private lazy val anyValTpe = typeOf[AnyVal]

  def analyze(unit: CompilationUnit): Unit = unit.body.foreach {
    case cd: ClassDef if cd.mods.hasFlag(Flag.IMPLICIT) =>
      val tpe = cd.symbol.typeSignature
      val primaryCtor = tpe.member(termNames.CONSTRUCTOR).alternatives.find(_.asMethod.isPrimaryConstructor)
      val paramLists = primaryCtor.map(_.asMethod.paramLists)
      val hasExactlyOneParam = paramLists.forall(lists => lists.size == 1 && lists.head.size == 1)

      if (!tpe.baseClasses.contains(anyValTpe.typeSymbol) && hasExactlyOneParam) {
        report(cd.pos, "Implicit classes should always extend AnyVal to become value classes")
      }
    case _ =>
  }
}
