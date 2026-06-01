package com.avsystem.commons
package serialization

import com.avsystem.commons.meta.OptionLike
import com.avsystem.commons.serialization.GenCodec.{FieldReadFailed, ReadFailure}

import scala.annotation.tailrec

object FieldValues {
  final val Empty = new FieldValues(new Array(0), new Array(0))
  private object NullMarker
}
final class FieldValues(
  private val fieldNames: Array[String],
  codecs: Array[GenCodec[?]],
  ofWhat: OptArg[String] = OptArg.Empty,
) {

  @tailrec private def fieldIndex(fieldName: String, idx: Int): Int =
    if (idx >= fieldNames.length) -1
    else if (fieldNames(idx) == fieldName) idx
    else fieldIndex(fieldName, idx + 1)

  private val values = new Array[Any](fieldNames.length)

  import FieldValues._

  def readField(input: FieldInput): Unit =
    if (!tryReadField(input)) {
      input.skip()
    }

  def tryReadField(input: FieldInput): Boolean = fieldIndex(input.fieldName, 0) match {
    case -1 => false
    case idx =>
      val value =
        try codecs(idx).read(input)
        catch {
          case NonFatal(e) =>
            ofWhat match {
              case OptArg(typeRepr) => throw FieldReadFailed(typeRepr, input.fieldName, e)
              case OptArg.Empty => throw new ReadFailure(s"Failed to read field ${input.fieldName}", e)
            }
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

  def getOpt[O, T](idx: Int, optionLike: OptionLike.Aux[O, T]): O = {
    val res = values(idx)
    if (res == null) optionLike.none
    else if (res.asInstanceOf[AnyRef] eq NullMarker) optionLike.some(null.asInstanceOf[T])
    else optionLike.some(res.asInstanceOf[T])
  }

  def rewriteFrom(other: FieldValues): Unit = {
    var oi = 0
    while (oi < other.fieldNames.length) {
      other.values(oi) match {
        case null =>
        case value =>
          fieldIndex(other.fieldNames(oi), 0) match {
            case -1 =>
            case i => values(i) = value
          }
      }
      oi += 1
    }
  }
}
