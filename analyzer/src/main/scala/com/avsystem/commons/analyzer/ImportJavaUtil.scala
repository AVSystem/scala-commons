package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.untpd.ImportSelector
import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Symbols.*

final class ImportJavaUtil extends AnalyzerRule("importJavaUtil"):
  import tpd.*
  override def transformOther(tree: tpd.Tree)(using Context): tpd.Tree =
    val pkg = Symbols.requiredPackage("java.util").defTree
    tree match
      case tree @ Import(pkg, _)=>
        report(
          "Don't import java.util: either import with rename (e.g. import java.{util => ju}) " +
            "or use type aliases from JavaInterop (e.g. JList, JSet, etc)",
          tree.symbol
        )(using tree.sourcePos)
      case tree =>

    end match

    tree

  end transformOther

end ImportJavaUtil
