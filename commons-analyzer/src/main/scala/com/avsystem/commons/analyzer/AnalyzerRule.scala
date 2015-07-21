package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

abstract class AnalyzerRule[C <: Global with Singleton](val global: C) {

  import global._

  def analyze(unit: CompilationUnit): Unit
}
