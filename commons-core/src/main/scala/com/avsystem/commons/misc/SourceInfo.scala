package com.avsystem.commons
package misc

/**
  * Macro-materialized implicit value that provides information about callsite source file position.
  * It can be used in runtime for logging and debugging purposes.
  * Similar to Scalactic's `Position`, but contains more information.
  */
case class SourceInfo(
  filePath: String,
  fileName: String,
  offset: Int,
  line: Int,
  column: Int,
  lineContent: String,
  enclosingSymbols: List[String]
)

object SourceInfo {
  def apply()(implicit si: SourceInfo): SourceInfo = si

  implicit def here: SourceInfo = macro macros.misc.MiscMacros.sourceInfo
}
