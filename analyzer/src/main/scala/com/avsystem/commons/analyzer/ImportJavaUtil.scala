package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.untpd.ImportSelector
import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Symbols.*
import tpd.*
import ImportJavaUtil.*

final class ImportJavaUtil extends AnalyzerRule("importJavaUtil"):

  override def transformOther(tree: Tree)(using Context): Tree = tree.tap {
    case Import(Ident(`javaPkgName`), selectors: List[ImportSelector]) if selectors.exists {
          case ImportSelector(Ident(`javaUtilPkgName`), EmptyTree, _) => true
          case _                                                      => false
        } =>
      report(
        "Don't import java.util: either import with rename (e.g. import java.{util => ju}) " +
          "or use type aliases from JavaInterop (e.g. JList, JSet, etc)",
        tree
      )
    case _ =>
  }

  end transformOther

end ImportJavaUtil

private object ImportJavaUtil:
  private final val javaPkgName = (ctx: Context) ?=> Symbols.requiredPackageRef("java").name
  private final val javaUtilPkgName = (ctx: Context) ?=> Symbols.requiredPackageRef("java.util").name

end ImportJavaUtil
