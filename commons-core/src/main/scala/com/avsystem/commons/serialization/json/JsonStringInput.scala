package com.avsystem.commons
package serialization.json

import com.avsystem.commons.annotation.explicitGenerics
import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization._
import com.avsystem.commons.serialization.json.JsonStringInput.{AfterElement, AfterElementNothing}

object JsonStringInput {
  @explicitGenerics def read[T: GenCodec](json: String): T =
    GenCodec.read[T](new JsonStringInput(new JsonReader(json)))

  private[json] object ObjectMarker {
    override def toString = "object"
  }
  private[json] object ListMarker {
    override def toString = "list"
  }

  trait AfterElement {
    def afterElement(): Unit
  }
  object AfterElementNothing extends AfterElement {
    def afterElement(): Unit = ()
  }
}

class JsonStringInput(reader: JsonReader, callback: AfterElement = AfterElementNothing) extends Input with AfterElement {
  private[this] var startIdx: Int = _
  private[this] var endIdx: Int = _

  private[this] val value: Any = {
    reader.skipWs()
    startIdx = reader.index
    val res = reader.parseValue()
    res match {
      case JsonStringInput.ListMarker | JsonStringInput.ObjectMarker =>
      case _ => afterElement()
    }
    res
  }

  def jsonType: String = value match {
    case JsonStringInput.ListMarker => "list"
    case JsonStringInput.ObjectMarker => "object"
    case _: String => "string"
    case _: Boolean => "boolean"
    case _: Int => "integer number"
    case _: Double => "decimal number"
    case null => "null"
  }

  private def expected(what: String) = throw new ReadFailure(s"Expected $what but got $jsonType: $value")

  private def matchOr[@specialized(Boolean) T: ClassTag](what: String): T = value match {
    case t: T => t
    case _ => expected(what)
  }

  private def matchNumericString[T](toNumber: String => T): T = value match {
    case ns: String =>
      Try(toNumber(ns)).recover { case t => throw new ReadFailure(s"Invalid number format: $ns", t) }.get
    case _ =>
      expected("numeric string")
  }

  def inputType: InputType = value match {
    case JsonStringInput.ListMarker => InputType.List
    case JsonStringInput.ObjectMarker => InputType.Object
    case null => InputType.Null
    case _ => InputType.Simple
  }

  def readNull(): Null = if (value == null) null else expected("null")
  def readString(): String = matchOr[String]("string")
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

  def readList(): JsonListInput = value match {
    case JsonStringInput.ListMarker => new JsonListInput(reader, this)
    case _ => expected("list")
  }

  def readObject(): JsonObjectInput = value match {
    case JsonStringInput.ObjectMarker => new JsonObjectInput(reader, this)
    case _ => expected("object")
  }

  def readRawJson(): String = {
    skip()
    reader.json.substring(startIdx, endIdx)
  }

  def skip(): Unit = value match {
    case JsonStringInput.ListMarker => readList().skipRemaining()
    case JsonStringInput.ObjectMarker => readObject().skipRemaining()
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

  def index: Int = i

  @inline def read(): Char = {
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

  def tryPass(ch: Char): Boolean =
    if (isNext(ch)) {
      advance()
      true
    } else false

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
      if (isNextDigit) {
        parseDigits()
      } else throw new ReadFailure(s"Expected a digit, got ${json.charAt(i)}")
    }

    val str = json.substring(start, i)
    str
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

  def parseValue(): Any =
    if (i < json.length) json.charAt(i) match {
      case '"' => parseString()
      case 't' => pass("true"); true
      case 'f' => pass("false"); false
      case 'n' => pass("null"); null
      case '[' => read(); JsonStringInput.ListMarker
      case '{' => read(); JsonStringInput.ObjectMarker
      case '-' => parseNumber()
      case c if Character.isDigit(c) => parseNumber()
      case c => throw new ReadFailure(s"Unexpected character: '${c.toChar}'")
    } else {
      throw new ReadFailure("EOF")
    }
}
