package com.avsystem.commons
package analyzer

import scala.tools.nsc.Global

abstract class AnalyzerRule[C <: Global with Singleton](val global: C) {

  import global._

  protected def classType(fullName: String) =
    try rootMirror.staticClass(fullName).asType.toType.erasure catch {
      case _: ScalaReflectionException => NoType
    }

  def analyze(unit: CompilationUnit): Unit
}
