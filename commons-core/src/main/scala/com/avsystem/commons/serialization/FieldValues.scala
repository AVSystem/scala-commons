package com.avsystem.commons
package serialization

import com.avsystem.commons.misc.CrossUtils._
import com.avsystem.commons.serialization.GenCodec.ReadFailure

import scala.annotation.tailrec

object FieldValues {
  final val Empty = new FieldValues(newNativeArray(0), newNativeArray(0))
  private object NullMarker
}
final class FieldValues(
  private val fieldNames: NativeArray[String], codecs: NativeArray[GenCodec[_]], ofWhat: OptArg[String] = OptArg.Empty) {

  @tailrec private def fieldIndex(fieldName: String, idx: Int): Int =
    if (idx >= fieldNames.length) -1
    else if (fieldNames(idx) == fieldName) idx
    else fieldIndex(fieldName, idx + 1)

  private val values = newNativeArray[Any](fieldNames.length)

  import FieldValues._

  def readField(input: FieldInput): Unit =
    if (!tryReadField(input)) {
      input.skip()
    }

  def tryReadField(input: FieldInput): Boolean = fieldIndex(input.fieldName, 0) match {
    case -1 => false
    case idx =>
      val value = try codecs(idx).read(input) catch {
        case NonFatal(e) => throw new ReadFailure(
          s"Failed to read field ${input.fieldName}${ofWhat.fold("")(what => s" of $what")}", e)
      }
      values(idx) = if (value == null) NullMarker else value
      true
  }

  def getOrElse[T](idx: Int, default: => T): T = {
    val res = values(idx)
    if (res == null) default
    else if (res.asInstanceOf[AnyRef] eq NullMarker) null.asInstanceOf[T]
    else res.asInstanceOf[T]
  }

  def rewriteFrom(other: FieldValues): Unit = {
    var oi = 0
    while (oi < other.fieldNames.length) {
      other.values(oi) match {
        case null =>
        case value => fieldIndex(other.fieldNames(oi), 0) match {
          case -1 =>
          case i => values(i) = value
        }
      }
      oi += 1
    }
  }
}
