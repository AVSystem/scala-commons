package com.avsystem.commons
package misc

import scala.annotation.tailrec
import scala.quoted.*

/** Macro-materialized implicit value that provides information about callsite source file position. It can be used in
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
  def apply()(implicit si: SourceInfo): SourceInfo = si

  inline implicit def here: SourceInfo = ${ hereImpl }

  private[misc] def hereImpl(using quotes: Quotes): Expr[SourceInfo] = {
    import quotes.reflect.*
    val pos = Position.ofMacroExpansion

    @tailrec
    def enclosingLoop(sym: Symbol, acc: Vector[String]): Seq[String] =
      if (sym == defn.RootClass) acc
      else enclosingLoop(sym.owner, acc :+ sym.name)

    '{
      SourceInfo(
        ${ Expr(pos.sourceFile.path) },
        ${ Expr(pos.sourceFile.name) },
        ${ Expr(pos.start) },
        ${ Expr(pos.startLine + 1) },
        ${ Expr(pos.startColumn + 1) },
        ${
          Expr(
            pos.sourceFile.content.flatMap(_.linesIterator.drop(pos.startLine).nextOption).getOrElse("<no content>")
          )
        },
        ${ Expr(enclosingLoop(Symbol.spliceOwner.owner, Vector.empty).toList) },
      )
    }
  }
}
