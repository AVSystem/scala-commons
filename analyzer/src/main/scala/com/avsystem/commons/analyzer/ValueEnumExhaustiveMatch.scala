package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Symbols.{NoSymbol, Symbol}

import scala.collection.mutable

class ValueEnumExhaustiveMatch(using Context) extends AnalyzerRule {
  val name: String = "valueEnumExhaustiveMatch"

  private val valueEnumClass: Symbol =
    Symbols.getClassIfDefined("com.avsystem.commons.misc.ValueEnum")

  private val valueEnumCompanionClass: Symbol =
    Symbols.getClassIfDefined("com.avsystem.commons.misc.ValueEnumCompanion")

  override def transformMatch(tree: tpd.Match)(using Context): tpd.Tree = {
    val veClass = valueEnumClass
    if (veClass != NoSymbol && tree.selector.tpe.widenDealias.classSymbol.derivesFrom(veClass)) {
      checkExhaustiveness(tree)
    }
    tree
  }

  private def checkExhaustiveness(tree: tpd.Match)(using Context): Unit = {
    val selectorTpe = tree.selector.tpe.widenDealias
    val classSym = selectorTpe.classSymbol
    val companion = classSym.companionModule

    val veCompClass = valueEnumCompanionClass
    if (companion == NoSymbol || veCompClass == NoSymbol) return
    if (!companion.info.derivesFrom(veCompClass)) return

    // Collect expected enum values: final, non-lazy, public vals of the selector type
    val unmatched = mutable.LinkedHashSet.from(
      companion.info.decls.iterator.filter { s =>
        s.isTerm && s.is(Flags.Final) && !s.is(Flags.Lazy) && s.isPublic && s.info.finalResultType <:< selectorTpe
      },
    )

    // Analyze each case to remove matched values
    tree.cases.foreach {
      case cd: tpd.CaseDef if cd.guard.isEmpty =>
        findMatchedEnums(cd.pat, unmatched)
      case _ =>
        unmatched.clear() // Guard present or unusual case -- assume covered
    }

    if (unmatched.nonEmpty) {
      val what =
        if (unmatched.size > 1) "inputs: " + unmatched.iterator.map(_.name.toString).mkString(", ")
        else "input: " + unmatched.head.name.toString
      report(tree, "match may not be exhaustive.\nIt would fail on the following " + what)
    }
  }

  private def findMatchedEnums(
    pattern: tpd.Tree,
    unmatched: mutable.Set[Symbol],
  )(using Context,
  ): Unit = pattern match {
    case tpd.Bind(_, body) => findMatchedEnums(body, unmatched)
    case tpd.Alternative(pats) => pats.foreach(findMatchedEnums(_, unmatched))
    case id: tpd.Ident if id.name == nme.WILDCARD => unmatched.clear()
    case _: tpd.Ident | _: tpd.Select => unmatched.remove(pattern.symbol)
    case _: tpd.Literal => // ignore literal patterns (e.g. null)
    case _ => unmatched.clear() // unknown pattern, assume exhaustive
  }
}
