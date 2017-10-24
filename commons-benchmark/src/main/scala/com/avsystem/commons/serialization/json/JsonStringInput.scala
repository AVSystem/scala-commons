package com.avsystem.commons
package serialization.json

import java.io.{Reader, StringReader}

import com.avsystem.commons.annotation.explicitGenerics
import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization.json.JsonStringInput.{AfterElement, AfterElementNothing}
import com.avsystem.commons.serialization.{FieldInput, GenCodec, Input, InputType, ListInput, ObjectInput}

object JsonStringInput {
  @explicitGenerics def read[T: GenCodec](json: String): T =
    GenCodec.read[T](new JsonStringInput(new JsonReader(new StringReader(json))))

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

class JsonStringInput(reader: JsonReader, callback: AfterElement = AfterElementNothing) extends Input {
  private[this] val value: Any = {
    val res = reader.parseValue()
    res match {
      case JsonStringInput.ListMarker | JsonStringInput.ObjectMarker =>
      case _ => callback.afterElement()
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

  private def expected(what: String) = throw new ReadFailure(s"Expected $what but got $jsonType")

  private def matchOr[T: ClassTag](what: String): T = value match {
    case t: T => t
    case _ => expected(what)
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
  def readInt(): Int = matchOr[Int]("integer number")

  def readLong(): Long = value match {
    case i: Int => i
    case s: String => s.toLong
    case _ => expected("integer number or numeric string")
  }

  def readDouble(): Double = matchOr[Double]("double number")

  def readBinary(): Array[Byte] = {
    val hex = matchOr[String]("hex string")
    val result = new Array[Byte](hex.length / 2)
    var i = 0
    while (i < result.length) {
      result(i) = ((reader.fromHex(hex.codePointAt(2 * i)) << 4) | reader.fromHex(hex.codePointAt(2 * i + 1))).toByte
      i += 1
    }
    result
  }

  def readList(): JsonListInput = value match {
    case JsonStringInput.ListMarker => new JsonListInput(reader, callback)
    case _ => expected("list")
  }

  def readObject(): JsonObjectInput = value match {
    case JsonStringInput.ObjectMarker => new JsonObjectInput(reader, callback)
    case _ => expected("object")
  }

  def skip(): Unit = value match {
    case JsonStringInput.ListMarker => readList().skipRemaining()
    case JsonStringInput.ObjectMarker => readObject().skipRemaining()
    case _ =>
  }
}

final class JsonStringFieldInput(val fieldName: String, reader: JsonReader, objectInput: JsonObjectInput)
  extends JsonStringInput(reader, objectInput) with FieldInput

final class JsonListInput(reader: JsonReader, callback: AfterElement) extends ListInput with AfterElement {
  private[this] var end = false

  prepareForNext(first = true)

  private def prepareForNext(first: Boolean): Unit = {
    end = reader.peekNoWs() == ']'
    if (end) {
      reader.read()
      callback.afterElement()
    } else if (!first) {
      reader.pass(',')
    }
  }

  def hasNext: Boolean = !end
  def nextElement(): JsonStringInput = {
    new JsonStringInput(reader, this)
  }

  def afterElement(): Unit = prepareForNext(first = false)
}

final class JsonObjectInput(reader: JsonReader, callback: AfterElement) extends ObjectInput with AfterElement {
  private[this] var end = false

  prepareForNext(first = true)

  private def prepareForNext(first: Boolean): Unit = {
    end = reader.peekNoWs() == '}'
    if (end) {
      reader.read()
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

final class JsonReader(reader: Reader) {
  private[this] var peeked: Int = -2

  def read(): Int =
    if (peeked != -2) {
      val res = peeked
      peeked = -2
      res
    } else reader.read()

  def peek(): Int =
    if (peeked != -2) peeked
    else {
      peeked = reader.read()
      peeked
    }

  def skipWs(): Unit = {
    while (Character.isWhitespace(peek())) {
      read()
    }
  }

  def peekNoWs(): Int = {
    skipWs()
    peek()
  }

  def pass(ch: Int): Unit = {
    val r = read()
    if (r != ch) throw new ReadFailure(s"'${ch.toChar}' expected, got ${if (r == -1) "EOF" else r.toChar}")
  }

  def tryPass(ch: Int): Boolean =
    if (peek() == ch) {
      read()
      true
    } else false

  private def pass(str: String): Unit = {
    var i = 0
    while (i < str.length) {
      if (read() != str.charAt(i)) {
        throw new ReadFailure(s"expected '$str'")
      }
      i += 1
    }
  }

  def fromHex(ch: Int): Int =
    if (ch >= 'A' && ch <= 'F') ch - 'A' + 10
    else if (ch >= 'a' && ch <= 'f') ch - 'a' + 10
    else if (ch >= '0' && ch <= '9') ch - '0'
    else throw new ReadFailure(s"Bad hex digit: ${ch.toChar}")

  private def readHex(): Int =
    fromHex(read())

  private def parseNumber(): Any = {
    val sb = new JStringBuilder
    var decimal = false

    def advance(): Unit =
      sb.appendCodePoint(read())

    if (peek() == '-') {
      advance()
    }

    def parseDigits(): Unit = {
      if (!Character.isDigit(peek())) {
        throw new ReadFailure("Expected digit")
      }
      while (Character.isDigit(peek())) {
        advance()
      }
    }

    if (peek() == '0') {
      advance()
    } else if (Character.isDigit(peek())) {
      parseDigits()
    } else throw new ReadFailure("Expected '-' or digit")

    if (peek() == '.') {
      decimal = true
      advance()
      parseDigits()
      if (peek() == 'e' || peek() == 'E') {
        advance()
        if (peek() == '-' || peek() == '+') {
          advance()
          parseDigits()
        } else throw new ReadFailure("Expected '+' or '-'")
      }
    }

    val str = sb.toString
    if (decimal) str.toDouble else str.toInt
  }

  def parseString(): String = {
    pass('"')
    val sb = new JStringBuilder
    var cont = true
    while (cont) {
      read() match {
        case '"' => cont = false
        case '\\' =>
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
        case c =>
          sb.append(c.toChar)
      }
    }
    sb.toString
  }

  def parseValue(): Any = peekNoWs() match {
    case '"' => parseString()
    case 't' => pass("true"); true
    case 'f' => pass("false"); false
    case 'n' => pass("null"); null
    case '[' => read(); JsonStringInput.ListMarker
    case '{' => read(); JsonStringInput.ObjectMarker
    case '-' => parseNumber()
    case c if Character.isDigit(c) => parseNumber()
    case c => throw new ReadFailure(s"Unexpected character: '${c.toChar}'")
  }
}
