package com.avsystem.commons
package serialization.json

import com.avsystem.commons.annotation.explicitGenerics
import com.avsystem.commons.misc.{AbstractValueEnum, AbstractValueEnumCompanion, EnumCtx}
import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization._
import com.avsystem.commons.serialization.json.JsonStringInput.{AfterElement, AfterElementNothing, JsonType}

object JsonStringInput {
  @explicitGenerics def read[T: GenCodec](json: String): T =
    GenCodec.read[T](new JsonStringInput(new JsonReader(json)))

  final class JsonType(implicit enumCtx: EnumCtx) extends AbstractValueEnum
  object JsonType extends AbstractValueEnumCompanion[JsonType] {
    final val list, `object`, number, string, boolean, `null`: Value = new JsonType
  }

  trait AfterElement {
    def afterElement(): Unit
  }
  object AfterElementNothing extends AfterElement {
    def afterElement(): Unit = ()
  }
}

class JsonStringInput(reader: JsonReader, callback: AfterElement = AfterElementNothing) extends Input with AfterElement {
  private[this] val startIdx: Int = reader.parseValue()
  private[this] var endIdx: Int = _

  reader.jsonType match {
    case JsonType.list | JsonType.`object` =>
    case _ => afterElement()
  }

  private def expected(what: String) = throw new ReadFailure(s"Expected $what but got ${reader.jsonType}: ${reader.currentValue}")

  private def matchOr[@specialized(Boolean) T: ClassTag](what: String): T = reader.currentValue match {
    case t: T => t
    case _ => expected(what)
  }

  private def matchNumericString[T](toNumber: String => T): T = reader.currentValue match {
    case ns: String if reader.jsonType == JsonType.number || ns == "Infinity" || ns == "-Infinity" || ns == "NaN" =>
      try toNumber(ns) catch {
        case e: NumberFormatException => throw new ReadFailure(s"Invalid number format: $ns", e)
      }
    case _ =>
      expected("number")
  }

  def inputType: InputType = reader.jsonType match {
    case JsonType.list => InputType.List
    case JsonType.`object` => InputType.Object
    case JsonType.`null` => InputType.Null
    case _ => InputType.Simple
  }

  def readNull(): Null = if (reader.currentValue == null) null else expected("null")
  def readString(): String = {
    val expectedTpe = "string"
    if (reader.jsonType != JsonType.string) expected(expectedTpe)
    matchOr[String](expectedTpe)
  }
  def readBoolean(): Boolean = matchOr[Boolean]("boolean")
  def readInt(): Int = matchNumericString(_.toInt)
  def readLong(): Long = matchNumericString(_.toLong)
  def readDouble(): Double = matchNumericString(_.toDouble)
  def readBinary(): Array[Byte] = {
    val hex = matchOr[String]("hex string")
    val result = new Array[Byte](hex.length / 2)
    var i = 0
    while (i < result.length) {
      result(i) = ((reader.fromHex(hex.charAt(2 * i)) << 4) | reader.fromHex(hex.charAt(2 * i + 1))).toByte
      i += 1
    }
    result
  }

  def readList(): JsonListInput = reader.jsonType match {
    case JsonType.list => new JsonListInput(reader, this)
    case _ => expected("list")
  }

  def readObject(): JsonObjectInput = reader.jsonType match {
    case JsonType.`object` => new JsonObjectInput(reader, this)
    case _ => expected("object")
  }

  def readRawJson(): String = {
    skip()
    reader.json.substring(startIdx, endIdx)
  }

  def skip(): Unit = reader.jsonType match {
    case JsonType.list => readList().skipRemaining()
    case JsonType.`object` => readObject().skipRemaining()
    case _ =>
  }

  override def afterElement(): Unit = {
    endIdx = reader.index
    callback.afterElement()
  }
}

final class JsonStringFieldInput(val fieldName: String, reader: JsonReader, objectInput: JsonObjectInput)
  extends JsonStringInput(reader, objectInput) with FieldInput

final class JsonListInput(reader: JsonReader, callback: AfterElement) extends ListInput with AfterElement {
  private[this] var end = false

  prepareForNext(first = true)

  private def prepareForNext(first: Boolean): Unit = {
    reader.skipWs()
    end = reader.isNext(']')
    if (end) {
      reader.advance()
      callback.afterElement()
    } else if (!first) {
      reader.pass(',')
    }
  }

  def hasNext: Boolean = !end
  def nextElement(): JsonStringInput = new JsonStringInput(reader, this)
  def afterElement(): Unit = prepareForNext(first = false)
}

final class JsonObjectInput(reader: JsonReader, callback: AfterElement) extends ObjectInput with AfterElement {
  private[this] var end = false

  prepareForNext(first = true)

  private def prepareForNext(first: Boolean): Unit = {
    reader.skipWs()
    end = reader.isNext('}')
    if (end) {
      reader.advance()
      callback.afterElement()
    } else if (!first) {
      reader.pass(',')
    }
  }

  def hasNext: Boolean = !end

  def nextField(): JsonStringFieldInput = {
    reader.skipWs()
    val fieldName = reader.parseString()
    reader.skipWs()
    reader.pass(':')
    new JsonStringFieldInput(fieldName, reader, this)
  }

  def afterElement(): Unit =
    prepareForNext(first = false)
}

final class JsonReader(val json: String) {
  private[this] var i: Int = 0
  private[this] var value: Any = _
  private[this] var tpe: JsonType = _

  def index: Int = i
  def currentValue: Any = value
  def jsonType: JsonType = tpe

  @inline private def read(): Char = {
    val res = json.charAt(i)
    advance()
    res
  }

  @inline def isNext(ch: Char): Boolean =
    i < json.length && json.charAt(i) == ch

  @inline def isNextDigit: Boolean =
    i < json.length && Character.isDigit(json.charAt(i))

  @inline def advance(): Unit = {
    i += 1
  }

  def skipWs(): Unit = {
    while (i < json.length && Character.isWhitespace(json.charAt(i))) {
      i += 1
    }
  }

  def pass(ch: Char): Unit = {
    val r = read()
    if (r != ch) throw new ReadFailure(s"'${ch.toChar}' expected, got ${if (r == -1) "EOF" else r.toChar}")
  }

  private def pass(str: String): Unit = {
    var j = 0
    while (j < str.length) {
      if (!isNext(str.charAt(j))) {
        throw new ReadFailure(s"expected '$str'")
      } else {
        advance()
      }
      j += 1
    }
  }

  def fromHex(ch: Char): Int =
    if (ch >= 'A' && ch <= 'F') ch - 'A' + 10
    else if (ch >= 'a' && ch <= 'f') ch - 'a' + 10
    else if (ch >= '0' && ch <= '9') ch - '0'
    else throw new ReadFailure(s"Bad hex digit: ${ch.toChar}")

  private def readHex(): Int =
    fromHex(read())

  private def parseNumber(): Any = {
    val start = i

    if (isNext('-')) {
      advance()
    }

    def parseDigits(): Unit = {
      if (!isNextDigit) {
        throw new ReadFailure(s"Expected digit, got ${json.charAt(i)}")
      }
      while (isNextDigit) {
        advance()
      }
    }

    if (isNext('0')) {
      advance()
    } else if (isNextDigit) {
      parseDigits()
    } else throw new ReadFailure(s"Expected '-' or digit, got ${json.charAt(i)}")

    if (isNext('.')) {
      advance()
      parseDigits()
    }

    //Double.MinPositiveValue.toString is 5e-324 in JS. Written by scientists, for scientists.
    if (isNext('e') || isNext('E')) {
      advance()
      if (isNext('-') || isNext('+')) {
        advance()
      }
      parseDigits()
    }

    json.substring(start, i)
  }

  def parseString(): String = {
    pass('"')
    var sb: JStringBuilder = null
    var plainStart = i
    var cont = true
    while (cont) {
      read() match {
        case '"' => cont = false
        case '\\' =>
          if (sb == null) {
            sb = new JStringBuilder
          }
          if (plainStart < i - 1) {
            sb.append(json, plainStart, i - 1)
          }
          val unesc = read() match {
            case '"' => '"'
            case '\\' => '\\'
            case 'b' => '\b'
            case 'f' => '\f'
            case 'n' => '\n'
            case 'r' => '\r'
            case 't' => '\t'
            case 'u' => ((readHex() << 12) + (readHex() << 8) + (readHex() << 4) + readHex()).toChar
          }
          sb.append(unesc)
          plainStart = i
        case _ =>
      }
    }
    if (sb != null) {
      sb.append(json, plainStart, i - 1)
      sb.toString
    } else {
      json.substring(plainStart, i - 1)
    }
  }

  /**
    * @return startIndex
    */
  def parseValue(): Int = {
    skipWs()
    val startIndex = index
    val (newValue, newTpe) = if (i < json.length) json.charAt(i) match {
      case '"' => (parseString(), JsonType.string)
      case 't' => pass("true"); (true, JsonType.boolean)
      case 'f' => pass("false"); (false, JsonType.boolean)
      case 'n' => pass("null"); (null, JsonType.`null`)
      case '[' => read(); (null, JsonType.list)
      case '{' => read(); (null, JsonType.`object`)
      case '-' => (parseNumber(), JsonType.number)
      case c if Character.isDigit(c) => (parseNumber(), JsonType.number)
      case c => throw new ReadFailure(s"Unexpected character: '${c.toChar}'")
    } else {
      throw new ReadFailure("EOF")
    }
    value = newValue
    tpe = newTpe
    startIndex
  }
}
