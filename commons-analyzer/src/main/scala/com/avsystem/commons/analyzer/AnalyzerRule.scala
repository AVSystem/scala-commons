package com.avsystem.commons
package analyzer

import java.io.{PrintWriter, StringWriter}

import scala.tools.nsc.Global
import scala.util.control.NonFatal

abstract class AnalyzerRule[C <: Global with Singleton](val global: C) {

  import global._

  protected def classType(fullName: String) =
    try rootMirror.staticClass(fullName).asType.toType.erasure catch {
      case _: ScalaReflectionException => NoType
    }

  protected def analyzeTree(fun: PartialFunction[Tree, Unit])(tree: Tree): Unit =
    try fun.applyOrElse(tree, (_: Tree) => ()) catch {
      case NonFatal(t) =>
        val sw = new StringWriter
        t.printStackTrace(new PrintWriter(sw))
        reporter.error(tree.pos, s"Analyzer rule $this failed: " + sw.toString)
    }

  def analyze(unit: CompilationUnit): Unit
}
