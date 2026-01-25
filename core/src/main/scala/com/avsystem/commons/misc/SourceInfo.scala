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

object SourceInfo extends SourceInfoMacros {
  def here()(implicit si: SourceInfo): SourceInfo = si
}
