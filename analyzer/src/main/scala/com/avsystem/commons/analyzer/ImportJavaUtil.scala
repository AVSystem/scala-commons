package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.untpd.ImportSelector
import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Symbols.*

final class ImportJavaUtil extends AnalyzerRule("importJavaUtil"):

  import tpd.*

  override def transformOther(tree: Tree)(using Context): Tree =
    val javaPkgName = Symbols.requiredPackageRef("java").name
    val javaUtilPkgName = Symbols.requiredPackageRef("java.util").name
    tree match
      case Import(Ident(`javaPkgName`), selectors: List[ImportSelector]) if selectors.exists {
        case ImportSelector(Ident(`javaUtilPkgName`), EmptyTree, _) => true
        case _ => false
      } => report(
        "Don't import java.util: either import with rename (e.g. import java.{util => ju}) " +
          "or use type aliases from JavaInterop (e.g. JList, JSet, etc)",
        tree,
      )
      case _ =>

    end match

    tree

  end transformOther

end ImportJavaUtil
