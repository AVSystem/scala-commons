package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts

final class OldStyleImplicitConversion extends AnalyzerRule("oldStyleImplicitConversion"):

  override def transformDefDef(tree: tpd.DefDef)(using Contexts.Context): tpd.Tree = tree.tap { tree =>
    if tree.symbol.isOldStyleImplicitConversion(directOnly = true) then
      report("old-style implicit conversions are deprecated, use Conversion instead", tree)
    else if tree.symbol.isOldStyleImplicitConversion(forImplicitClassOnly = true) then
      report("old-style implicit conversions are deprecated, use extension", tree)

    end if
  }

end OldStyleImplicitConversion
