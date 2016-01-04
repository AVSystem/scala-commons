package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

class ImportJavaUtil[C <: Global with Singleton](g: C) extends AnalyzerRule(g) {

  import global._

  def analyze(unit: CompilationUnit): Unit = {
    unit.body.foreach(analyzeTree {
      case tree@q"import java.util" =>
        reporter.error(tree.pos, "Don't import java.util: either import with rename (e.g. import java.{util => ju}) " +
          "or use type aliases from JavaInterop (e.g. JList, JSet, etc)")
    })
  }
}
