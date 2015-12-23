package com.avsystem.commons
package analyzer

import scala.collection.mutable
import scala.tools.nsc.Global

/**
  * See <a href="https://issues.scala-lang.org/browse/SI-7046">SI-7046</a>
  */
class DetectSI7046[C <: Global with Singleton](g: C) extends AnalyzerRule(g) {

  import global._

  val checkKnownSubtypesSym = rootMirror.staticClass("com.avsystem.commons.annotation.checkKnownSubtypes")

  def allCurrentlyKnownSubclasses(sym: Symbol): Set[Symbol] =
    if (sym.isClass) {
      val directSubclasses = sym.asClass.knownDirectSubclasses
      directSubclasses.flatMap(allCurrentlyKnownSubclasses) + sym
    } else Set.empty

  def analyze(unit: CompilationUnit) = {
    val alreadyCheckedFor: mutable.Set[Position] = new mutable.HashSet[Position]

    for {
      tree <- unit.body if tree.tpe != null
      annot <- tree.tpe.annotations
    } annot.tree match {
      case tree@Apply(Select(New(pre), termNames.CONSTRUCTOR), List(Literal(Constant(actualSize: Int))))
        if pre.symbol == checkKnownSubtypesSym =>

        val hierarchyRoot = pre.tpe.typeArgs.head.typeSymbol
        if (alreadyCheckedFor.add(tree.pos)) {
          val expectedSize = allCurrentlyKnownSubclasses(hierarchyRoot).size
          if (expectedSize != actualSize) {
            reporter.error(tree.pos,
              s"""`knownDirectSubclasses` for $hierarchyRoot used in a macro did not correctly detect all subclasses.
                  |This is caused probably by a limitation of the Scala compiler described in SI-7046.
                  |Common workaround is to move the macro invocation to the end of the file (after entire sealed hierarchy has been defined).
               """.stripMargin)
          }
        }
      case _ =>
    }
  }
}
