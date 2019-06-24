package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

class ImportJavaUtil(g: Global) extends AnalyzerRule(g, "importJavaUtil") {

  import global._

  def analyze(unit: CompilationUnit): Unit = {
    unit.body.foreach(analyzeTree {
      case tree@q"import java.util" =>
        report(tree.pos, "Don't import java.util: either import with rename (e.g. import java.{util => ju}) " +
          "or use type aliases from JavaInterop (e.g. JList, JSet, etc)")
    })
  }
}
