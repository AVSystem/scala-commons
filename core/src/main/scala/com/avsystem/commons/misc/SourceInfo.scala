package com.avsystem.commons
package misc

/**
 * Macro-materialized implicit value that provides information about callsite source file position. It can be used in
 * runtime for logging and debugging purposes. Similar to Scalactic's `Position`, but contains more information.
 */
case class SourceInfo(
  filePath: String,
  fileName: String,
  offset: Int,
  line: Int,
  column: Int,
  lineContent: String,
  enclosingSymbols: List[String],
) {
  override def equals(obj: Any): Boolean = obj match {
    case otherInfo: SourceInfo => filePath == otherInfo.filePath && offset == otherInfo.offset
    case _ => false
  }

  override def hashCode: Int =
    (filePath, offset).hashCode
}

object SourceInfo {
  def here()(using si: SourceInfo): SourceInfo = si

  inline given here: SourceInfo = ${ hereImpl }
  private def hereImpl(using quotes: Quotes): Expr[SourceInfo] = {
    import quotes.reflect.*
    val pos = Position.ofMacroExpansion
    '{
      SourceInfo(
        ${ Expr(pos.sourceFile.path) },
        ${ Expr(pos.sourceFile.name) },
        ${ Expr(pos.start) },
        ${ Expr(pos.startLine + 1) },
        ${ Expr(pos.startColumn + 1) },
        ${ Expr("") }, // todo
        ${ Expr(Nil) }, // todo
      )
    }
  }
}
