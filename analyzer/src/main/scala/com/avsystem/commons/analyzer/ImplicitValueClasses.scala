package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

class ImplicitValueClasses(g: Global) extends AnalyzerRule(g, "implicitValueClasses", Level.Warn) {

  import global.*
  import definitions.*

  private lazy val defaultClasses = Set[Symbol](AnyClass, AnyValClass, ObjectClass)

  private lazy val reportOnNestedClasses = argument match {
    case null => false
    case a => a.toBoolean
  }

  private lazy val message = "Implicit classes should always extend AnyVal to become value classes" +
    (if (reportOnNestedClasses) ". Nested classes should be extracted to top-level objects" else "")

  def analyze(unit: CompilationUnit): Unit = unit.body.foreach {
    case cd: ClassDef if cd.mods.hasFlag(Flag.IMPLICIT) =>
      val tpe = cd.symbol.typeSignature
      val primaryCtor = tpe.member(termNames.CONSTRUCTOR).alternatives.find(_.asMethod.isPrimaryConstructor)
      val paramLists = primaryCtor.map(_.asMethod.paramLists)
      val inheritsAnyVal = tpe.baseClasses contains AnyValClass

      def inheritsOtherClass = tpe.baseClasses.exists { cls =>
        def isDefault = defaultClasses contains cls
        def isUniversalTrait = cls.isTrait && cls.superClass == AnyClass

        cls != cd.symbol && !isDefault && !isUniversalTrait
      }
      def hasExactlyOneParam = paramLists.forall(lists => lists.size == 1 && lists.head.size == 1)

      if (!inheritsAnyVal && !inheritsOtherClass && hasExactlyOneParam) {
        def isNestedClass =
          //implicit classes are always nested classes, so we want to check if the outer class's an object
          /*cd.symbol.isNestedClass &&*/ !cd.symbol.isStatic

        if (reportOnNestedClasses || !isNestedClass)
          report(cd.pos, message)
        else
          report(cd.pos, message, level = Level.Info)
      }
    case _ =>
  }
}
