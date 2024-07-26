package com.avsystem.commons
package serialization.nativejs

import com.avsystem.commons.annotation.explicitGenerics
import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization.*
import com.avsystem.commons.serialization.json.RawJson

import scala.scalajs.js
import scala.scalajs.js.JSON

class NativeJsonInput(value: js.Any, options: NativeFormatOptions) extends InputAndSimpleInput { self =>
  private def read[T](expected: String)(matcher: PartialFunction[Any, T]): T =
    matcher.applyOrElse(value, (_: Any) => throw new ReadFailure(s"$expected expected."))

  override def readNull(): Boolean =
    value == null

  override def readString(): String =
    read("String") {
      case s: String => s
    }

  override def readDouble(): Double =
    read("Double") {
      case v: Double => v
    }

  override def readInt(): Int =
    read("Int") {
      case v: Int => v
    }

  override def readLong(): Long = {
    def longFromString(s: String): Long =
      try s.toLong
      catch {
        case e: NumberFormatException => throw new ReadFailure(s"Cannot read Long", e)
      }
    (value: Any) match {
      case s: String => longFromString(s)
      case i: Int => i
      case d: Double if d.isWhole => d.toLong
      case b: js.BigInt => longFromString(b.toString)
      case b if js.typeOf(b) == "bigint" => longFromString(b.asInstanceOf[js.BigInt].toString)
      case o => throw new ReadFailure(s"Cannot read Long, got: ${js.typeOf(o)}")
    }
  }

  override def readBigInt(): BigInt = {
    def fail = throw new ReadFailure(s"BigInt expected.")
    (value: Any) match {
      case s: String =>
        try BigInt(s)
        catch {
          case _: NumberFormatException => fail
        }
      case i: Int => BigInt(i)
      case d: Double if d.isWhole => BigInt(d.toLong)
      case _ => fail
    }
  }

  override def readBigDecimal(): BigDecimal = {
    def fail = throw new ReadFailure(s"BigDecimal expected.")
    (value: Any) match {
      case s: String =>
        try BigDecimal(s)
        catch {
          case _: NumberFormatException => fail
        }
      case i: Int => BigDecimal(i)
      case d: Double => BigDecimal(d)
      case _ => fail
    }
  }

  override def readBoolean(): Boolean =
    read("Boolean") {
      case v: Boolean => v
    }

  override def readList(): ListInput =
    read("List") {
      case array: js.Array[js.Any @unchecked] => new JsonListInput(array, options)
    }

  override def readObject(): ObjectInput =
    read("Object") {
      case obj: js.Object => new JsonObjectInput(obj.asInstanceOf[js.Dictionary[js.Any]], options)
    }

  override def readTimestamp(): Long = options.dateFormat match {
    case NativeDateFormat.RawString | NativeDateFormat.JsNumber =>
      readLong() // lenient behaviour, accept any value that can be interpreted as Long
    case NativeDateFormat.JsDate =>
      read("js.Date") {
        case v: js.Date => v.getTime().toLong
      }
  }

  override def skip(): Unit = ()

  override def readBinary(): Array[Byte] =
    readList().iterator(_.readSimple().readInt().toByte).toArray

  override def readCustom[T](typeMarker: TypeMarker[T]): Opt[T] =
    typeMarker match {
      case RawJson => JSON.stringify(readRaw()).opt
      case _ => Opt.Empty
    }

  def readRaw(): js.Any = value
}

final class JsonListInput(list: js.Array[js.Any], options: NativeFormatOptions) extends ListInput {
  var it = 0

  override def hasNext: Boolean =
    it < list.length

  override def nextElement(): Input = {
    js.BigInt
    val in = new NativeJsonInput(list(it), options)
    it += 1
    in
  }
}

final class JsonObjectInput(dict: js.Dictionary[js.Any], options: NativeFormatOptions) extends ObjectInput {
  val it = dict.keysIterator

  override def hasNext: Boolean =
    it.hasNext

  override def peekField(name: String): Opt[FieldInput] =
    if (dict.contains(name)) Opt(new NativeJsonFieldInput(name, dict(name), options)) else Opt.Empty

  override def nextField(): FieldInput = {
    val key = it.next()
    new NativeJsonFieldInput(key, dict.apply(key), options)
  }
}

final class NativeJsonFieldInput(
  val fieldName: String,
  value: js.Any,
  options: NativeFormatOptions,
) extends NativeJsonInput(value, options)
  with FieldInput

object NativeJsonInput {
  @explicitGenerics
  def read[T: GenCodec](value: js.Any, options: NativeFormatOptions = NativeFormatOptions.RawString): T =
    GenCodec.read[T](new NativeJsonInput(value, options))

  @explicitGenerics
  def readString[T: GenCodec](value: String, options: NativeFormatOptions = NativeFormatOptions.RawString): T =
    read[T](JSON.parse(value), options)
}
