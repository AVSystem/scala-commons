package com.avsystem.commons
package hocon

import scala.annotation.tailrec
import scala.collection.mutable

object SourceFile {
  def apply(contents: String, name: String = "<input>"): SourceFile =
    new SourceFile(contents, name)
}
final class SourceFile(val contents: String, val name: String) {
  private lazy val lineOffsets: Array[Int] = {
    val b = new mutable.ArrayBuilder.ofInt
    b += 0
    @tailrec def loop(i: Int): Unit =
      if (i < contents.length()) contents.charAt(i) match {
        case '\n' =>
          b += i + 1
          loop(i + 1)
        case _ =>
          loop(i + 1)
      }
    loop(0)
    b.result()
  }

  def length: Int =
    contents.length()

  // number of newline characters + 1
  def lineCount: Int =
    lineOffsets.length

  // excluding the newline character
  def lineLength(line: Int): Int =
    if (line < 0 || line >= lineCount) 0
    else if (line == lineCount - 1) contents.length() - lineOffsets(line)
    else lineOffsets(line + 1) - lineOffsets(line) - 1

  def offsetFor(line: Int, column: Int): Int =
    if (line < 0) 0
    else if (line >= lineCount) contents.length
    else lineOffsets(line) + math.min(column, lineLength(line))

  def lineOf(offset: Int): Int =
    if (offset < 0) 0
    else if (offset >= contents.length()) lineCount - 1
    else {
      @tailrec def binsearch(beg: Int, end: Int): Int =
        if (beg > end) end
        else {
          val mid = (beg + end) / 2
          if (lineOffsets(mid) > offset) binsearch(beg, mid - 1)
          else binsearch(mid + 1, end)
        }
      binsearch(0, lineCount - 1)
    }

  def coordsOf(offset: Int): (Int, Int) =
    if (offset < 0) (0, 0)
    else if (offset >= contents.length()) (lineCount - 1, contents.length() - lineOffsets(lineCount - 1))
    else {
      val line = lineOf(offset)
      val column = offset - lineOffsets(line)
      (line, column)
    }

  def text(start: Int, end: Int): String = {
    val sb = new JStringBuilder
    sb.append(contents, start, end)
    sb.toString
  }

  def pos(start: Int, end: Int): SourcePos =
    SourcePos(this, start, end)

  def pos(startLine: Int, startColumn: Int, endLine: Int, endColumn: Int): SourcePos =
    SourcePos(this, offsetFor(startLine, startColumn), offsetFor(endLine, endColumn))

  def point(line: Int, column: Int): SourcePos = {
    val offset = offsetFor(line, column)
    SourcePos(this, offset, offset)
  }
}

final case class SourcePos(input: SourceFile, start: Int, end: Int) {
  require(start >= 0 && start <= input.length)
  require(end >= start && end <= input.length)

  private lazy val startCoords = input.coordsOf(start)
  private lazy val endCoords = input.coordsOf(end)

  override def toString: String = {
    val fromRepr = s"${startLine + 1}:${startColumn + 1}"
    val toRepr = if (isEmpty) "" else s"-${endLine + 1}:${endColumn + 1}"
    s"${input.name}:$fromRepr$toRepr"
  }

  lazy val text: String = input.text(start, end)

  /** 0-based starting line number */
  def startLine: Int = startCoords._1

  /** 0-based starting column number */
  def startColumn: Int = startCoords._2

  /** 0-based ending line number */
  def endLine: Int = endCoords._1

  /** 0-based ending column number */
  def endColumn: Int = endCoords._2

  def isEmpty: Boolean = end == start
  def nonEmpty: Boolean = end > start
  def length: Int = end - start

  def emptyStartPos: SourcePos = SourcePos(input, start, start)
  def emptyEndPos: SourcePos = SourcePos(input, end, end)

  def join(other: SourcePos): SourcePos =
    SourcePos(input, math.min(start, other.start), math.max(end, other.end))

  def until(other: SourcePos): SourcePos =
    SourcePos(input, start, math.max(start, other.start))

  def contains(other: SourcePos): Boolean =
    input == other.input && start <= other.start && end >= other.end

  def shift(shift: Int): SourcePos =
    SourcePos(input, start + shift, end + shift)

  def shiftStart(shift: Int): SourcePos =
    SourcePos(input, start + shift, end)

  def shiftEnd(shift: Int): SourcePos =
    SourcePos(input, start, end + shift)
}
