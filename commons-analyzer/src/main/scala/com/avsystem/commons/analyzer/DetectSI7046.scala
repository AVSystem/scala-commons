package com.avsystem.commons
package analyzer

import scala.collection.mutable
import scala.tools.nsc.Global

/**
  * See <a href="https://issues.scala-lang.org/browse/SI-7046">SI-7046</a>
  */
class DetectSI7046[C <: Global with Singleton](g: C) extends AnalyzerRule(g) {

  import global._

  val checkKnownSubtypesSym =
    try rootMirror.staticClass("com.avsystem.commons.annotation.checkKnownSubtypes")
    catch {
      case _: ScalaReflectionException => NoSymbol
    }

  def allCurrentlyKnownSubclasses(sym: Symbol): Set[Symbol] =
    if (sym.isClass) {
      val directSubclasses = sym.asClass.knownDirectSubclasses
      directSubclasses.flatMap(allCurrentlyKnownSubclasses) + sym
    } else Set.empty

  def analyze(unit: CompilationUnit) = if (checkKnownSubtypesSym != NoSymbol) {
    val alreadyCheckedFor: mutable.Set[Position] = new mutable.HashSet[Position]

    unit.body.foreach(analyzeTree {
      case tree if tree.tpe != null => tree.tpe.annotations.map(_.tree).foreach {
        case annotTree@Apply(Select(New(pre), termNames.CONSTRUCTOR), List(Literal(Constant(actualSize: Int))))
          if pre.symbol == checkKnownSubtypesSym =>

          val hierarchyRoot = pre.tpe.typeArgs.head.typeSymbol
          if (alreadyCheckedFor.add(tree.pos)) {
            val expectedSize = allCurrentlyKnownSubclasses(hierarchyRoot).size
            if (expectedSize != actualSize) {
              reporter.error(tree.pos,
                s"""`knownDirectSubclasses` for $hierarchyRoot used in a macro did not correctly detect all subclasses.
                    |This is caused by a limitation of Scala macro engine described in SI-7046.
                    |Common workaround is to move the macro invocation after entire sealed hierarchy has been defined
                    |(e.g. at the end of the file) so that the macro sees the entire hierarchy already typechecked.
                 """.stripMargin)
            }
          }
        case _ =>
      }
    })
  }
}
