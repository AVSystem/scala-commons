package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeTraverser
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol

class ImportJavaUtil extends AnalyzerRule {
  val name: String = "importJavaUtil"

  override def transformUnit(tree: tpd.Tree)(using Context): tpd.Tree = {
    object ImportChecker extends TreeTraverser {
      override def traverse(tree: tpd.Tree)(using Context): Unit = tree match {
        case imp: tpd.Import if imp.expr.symbol != NoSymbol =>
          val qualPath = imp.expr.symbol.fullName.toString
          // In Scala 3, `import java.util` is represented as qualifier=java, selector=util
          // Check if any non-renamed selector imports `util` from `java` (i.e. import java.util)
          val importsJavaUtil = qualPath == "java" && imp.selectors.exists { sel =>
            sel.name.toString == "util" && sel.renamed.isEmpty
          }
          if (importsJavaUtil) {
            report(
              imp,
              "Don't import java.util: either import with rename (e.g. import java.{util => ju}) " +
                "or use type aliases from JavaInterop (e.g. JList, JSet, etc)",
            )
          }
          traverseChildren(tree)
        case _ =>
          traverseChildren(tree)
      }
    }
    ImportChecker.traverse(tree)
    tree
  }
}
