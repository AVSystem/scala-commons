package com.avsystem.commons
package analyzer

import scala.collection.mutable
import scala.tools.nsc.Global
import scala.collection.compat._

class ValueEnumExhaustiveMatch(g: Global) extends AnalyzerRule(g, "valueEnumExhaustiveMatch") {

  import global._

  lazy val valueEnumTpe: Type = classType("com.avsystem.commons.misc.ValueEnum")
  lazy val ExistentialType(_, TypeRef(miscPackageTpe, valueEnumCompanionSym, _)) =
    classType("com.avsystem.commons.misc.ValueEnumCompanion")

  def analyze(unit: CompilationUnit): Unit = if (valueEnumTpe != NoType) {
    unit.body.foreach(analyzeTree {
      case tree@Match(selector, cases) if selector.tpe <:< valueEnumTpe =>
        val expectedCompanionTpe = TypeRef(miscPackageTpe, valueEnumCompanionSym, List(selector.tpe))
        val companion = selector.tpe.typeSymbol.companion
        val companionTpe = companion.toType
        if (companionTpe <:< expectedCompanionTpe) {
          val unmatched = companionTpe.decls.iterator
            .filter(s => s.isVal && s.isFinal && !s.isLazy && s.typeSignature <:< selector.tpe)
            .map(_.getterIn(companion)).filter(_.isPublic).to(mutable.LinkedHashSet)

          def findMatchedEnums(pattern: Tree): Unit = pattern match {
            case Bind(_, body) => findMatchedEnums(body)
            case Alternative(patterns) => patterns.foreach(findMatchedEnums)
            case Ident(termNames.WILDCARD) => unmatched.clear()
            case _: Ident | _: Select => unmatched.remove(pattern.symbol)
            case _: Literal =>
            case _ => unmatched.clear()
          }

          cases.iterator.foreach {
            case CaseDef(pattern, EmptyTree, _) => findMatchedEnums(pattern)
            case _ => unmatched.clear()
          }

          if (unmatched.nonEmpty) {
            val what =
              if (unmatched.size > 1) "inputs: " + unmatched.map(_.nameString).mkString(", ")
              else "input: " + unmatched.head.nameString
            report(tree.pos, "match may not be exhaustive.\nIt would fail on the following " + what)
          }
        }
    })
  }
}
