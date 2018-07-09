package com.avsystem.commons
package serialization.json

import com.avsystem.commons.annotation.explicitGenerics
import com.avsystem.commons.misc.{AbstractValueEnum, AbstractValueEnumCompanion, EnumCtx}
import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization._
import com.avsystem.commons.serialization.json.JsonStringInput.{AfterElement, AfterElementNothing, JsonType}

import scala.collection.mutable

object JsonStringInput {
  @explicitGenerics def read[T: GenCodec](json: String, options: JsonOptions = JsonOptions.Default): T =
    GenCodec.read[T](new JsonStringInput(new JsonReader(json), options))

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

class JsonStringInput(reader: JsonReader, options: JsonOptions = JsonOptions.Default,
  callback: AfterElement = AfterElementNothing
) extends Input with AfterElement {

  private[this] val startIdx: Int = reader.parseValue()
  private[this] var endIdx: Int = _

  reader.jsonType match {
    case JsonType.list | JsonType.`object` =>
    case _ => afterElement()
  }

  private def expectedError(tpe: JsonType) =
    throw new ReadFailure(s"Expected $tpe but got ${reader.jsonType}: ${reader.currentValue}")

  private def checkedValue[T](jsonType: JsonType): T = {
    if (reader.jsonType != jsonType) expectedError(jsonType)
    else reader.currentValue.asInstanceOf[T]
  }

  private def matchNumericString[T](toNumber: String => T): T = {
    if (reader.jsonType == JsonType.number ||
      (reader.jsonType == JsonType.string &&
        (reader.currentValue == "Infinity" || reader.currentValue == "-Infinity" || reader.currentValue == "NaN"))) {
      val str = reader.currentValue.asInstanceOf[String]
      try toNumber(str) catch {
        case e: NumberFormatException => throw new ReadFailure(s"Invalid number format: $str", e)
      }
    } else expectedError(JsonType.number)
  }

  def isNull: Boolean = reader.jsonType == JsonType.`null`
  def readNull(): Null = checkedValue[Null](JsonType.`null`)
  def readString(): String = checkedValue[String](JsonType.string)
  def readBoolean(): Boolean = checkedValue[Boolean](JsonType.boolean)
  def readInt(): Int = matchNumericString(_.toInt)
  def readLong(): Long = matchNumericString(_.toLong)
  def readDouble(): Double = matchNumericString(_.toDouble)
  def readBigInt(): BigInt = matchNumericString(BigInt(_))
  def readBigDecimal(): BigDecimal = matchNumericString(BigDecimal(_, options.mathContext))

  override def readTimestamp(): Long = options.dateFormat match {
    case JsonDateFormat.EpochMillis => readLong()
    case JsonDateFormat.IsoInstant => IsoInstant.parse(readString())
  }

  def readBinary(): Array[Byte] = options.binaryFormat match {
    case JsonBinaryFormat.ByteArray =>
      val builder = new mutable.ArrayBuilder.ofByte
      val li = readList()
      while (li.hasNext) {
        builder += li.nextElement().readByte()
      }
      builder.result()
    case JsonBinaryFormat.HexString =>
      val hex = checkedValue[String](JsonType.string)
      val result = new Array[Byte](hex.length / 2)
      var i = 0
      while (i < result.length) {
        result(i) = ((reader.fromHex(hex.charAt(2 * i)) << 4) | reader.fromHex(hex.charAt(2 * i + 1))).toByte
        i += 1
      }
      result
  }

  def readList(): JsonListInput = reader.jsonType match {
    case JsonType.list => new JsonListInput(reader, options, this)
    case _ => expectedError(JsonType.list)
  }

  def readObject(): JsonObjectInput = reader.jsonType match {
    case JsonType.`object` => new JsonObjectInput(reader, options, this)
    case _ => expectedError(JsonType.`object`)
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

final class JsonStringFieldInput(
  val fieldName: String, reader: JsonReader, options: JsonOptions, objectInput: JsonObjectInput)
  extends JsonStringInput(reader, options, objectInput) with FieldInput

final class JsonListInput(reader: JsonReader, options: JsonOptions, callback: AfterElement)
  extends ListInput with AfterElement {

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
  def nextElement(): JsonStringInput = new JsonStringInput(reader, options, this)
  def afterElement(): Unit = prepareForNext(first = false)
}

final class JsonObjectInput(reader: JsonReader, options: JsonOptions, callback: AfterElement)
  extends ObjectInput with AfterElement {

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
    new JsonStringFieldInput(fieldName, reader, options, this)
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

  private def parseNumber(): String = {
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
    @inline def update(newValue: Any, newTpe: JsonType): Unit = {
      value = newValue
      tpe = newTpe
    }
    skipWs()
    val startIndex = index
    if (i < json.length) json.charAt(i) match {
      case '"' => update(parseString(), JsonType.string)
      case 't' => pass("true"); update(true, JsonType.boolean)
      case 'f' => pass("false"); update(false, JsonType.boolean)
      case 'n' => pass("null"); update(null, JsonType.`null`)
      case '[' => read(); update(null, JsonType.list)
      case '{' => read(); update(null, JsonType.`object`)
      case '-' => update(parseNumber(), JsonType.number)
      case c if Character.isDigit(c) => update(parseNumber(), JsonType.number)
      case c => throw new ReadFailure(s"Unexpected character: '${c.toChar}'")
    } else throw new ReadFailure("EOF")
    startIndex
  }
}
