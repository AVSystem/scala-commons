package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

class ConstantDeclarations(g: Global) extends AnalyzerRule(g, "constantDeclarations") {

  import global._

  def analyze(unit: CompilationUnit): Unit = unit.body.foreach {
    case t@ValDef(mods, name, tpt, rhs)
      if t.symbol.hasGetter && t.symbol.owner.isEffectivelyFinal =>

      val getter = t.symbol.getterIn(t.symbol.owner)
      if (getter.isPublic && getter.isStable && getter.overrides.isEmpty) {
        val constantValue = rhs.tpe match {
          case ConstantType(_) => true
          case _ => false
        }

        val firstChar = name.toString.charAt(0)
        if (constantValue && (firstChar.isLower || !getter.isFinal)) {
          report(t.pos, "a constant should be declared as a `final val` with an UpperCamelCase name")
        }
        if (firstChar.isUpper && !getter.isFinal) {
          report(t.pos, "a constant with UpperCamelCase name should be declared as a `final val`")
        }
        if (getter.isFinal && constantValue && !(tpt.tpe =:= rhs.tpe)) {
          report(t.pos, "a constant with a literal value should not have an explicit type annotation")
        }
      }
    case _ =>
  }
}
