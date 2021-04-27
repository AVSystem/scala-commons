package com.avsystem.commons
package serialization.json

import com.avsystem.commons.annotation.explicitGenerics
import com.avsystem.commons.misc.CrossUtils
import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization._
import com.avsystem.commons.serialization.json.JsonStringInput._

import scala.annotation.tailrec
import scala.collection.mutable

object JsonStringInput {
  @explicitGenerics def read[T: GenCodec](json: String, options: JsonOptions = JsonOptions.Default): T =
    GenCodec.read[T](new JsonStringInput(new JsonReader(json), options))

  trait AfterElement {
    def afterElement(): Unit
  }
  object AfterElementNothing extends AfterElement {
    def afterElement(): Unit = ()
  }

  class ParseException(msg: String, cause: Throwable = null)
    extends ReadFailure(msg, cause)
}

class JsonStringInput(
  reader: JsonReader, options: JsonOptions = JsonOptions.Default, callback: AfterElement = AfterElementNothing
) extends InputAndSimpleInput with AfterElement {

  private[this] val startIdx: Int = reader.parseValue()
  private[this] var endIdx: Int = _

  reader.jsonType match {
    case JsonType.list | JsonType.`object` =>
    case _ => afterElement()
  }

  private def expectedError(tpe: JsonType) =
    throw new ParseException(s"Expected $tpe but got ${reader.jsonType}: ${reader.currentValue} ${reader.posInfo(startIdx)}")

  private def checkedValue[T](jsonType: JsonType): T =
    if (reader.jsonType != jsonType) expectedError(jsonType)
    else reader.currentValue.asInstanceOf[T]

  def readNull(): Boolean = reader.jsonType == JsonType.`null`
  def readString(): String = checkedValue[String](JsonType.string)
  def readBoolean(): Boolean = checkedValue[Boolean](JsonType.boolean)

  private def isInteger(str: String): Boolean = {
    @tailrec def loop(i: Int): Boolean =
      i >= str.length || str.charAt(i).isDigit && loop(i + 1)
    str.nonEmpty && {
      if (str.charAt(0) == '-') str.length > 1 && loop(1)
      else loop(0)
    }
  }

  private def parseNumber[T](parse: String => T): T = {
    val str = reader.jsonType match {
      // accepting JSON strings as numbers (also because they may be Infinity/-Infinity/NaN)
      case JsonType.number | JsonType.string => reader.currentValue.asInstanceOf[String]
      case _ => expectedError(JsonType.number)
    }
    try parse(str) catch {
      case e: NumberFormatException =>
        throw new ParseException(s"Invalid number format: $str ${reader.posInfo(startIdx)}", e)
    }
  }

  override def readByte(): Byte = parseNumber { str =>
    if (isInteger(str)) str.toByte
    else {
      val dbl = str.toDouble
      if (dbl.isValidByte) dbl.toByte
      else throw new NumberFormatException(str)
    }
  }

  override def readShort(): Short = parseNumber { str =>
    if (isInteger(str)) str.toShort
    else {
      val dbl = str.toDouble
      if (dbl.isValidShort) dbl.toShort
      else throw new NumberFormatException(str)
    }
  }

  def readInt(): Int = parseNumber { str =>
    if (isInteger(str)) str.toInt
    else {
      val dbl = str.toDouble
      if (dbl.isValidInt) dbl.toInt
      else throw new NumberFormatException(str)
    }
  }

  def readLong(): Long = parseNumber { str =>
    if (isInteger(str)) str.toLong
    else {
      val bd = BigDecimal(str, options.mathContext)
      if (bd.isValidLong) bd.toLong
      else throw new NumberFormatException(str)
    }
  }

  override def readFloat(): Float =
    parseNumber(_.toFloat)

  def readDouble(): Double =
    parseNumber(_.toDouble)

  def readBigInt(): BigInt = parseNumber { str =>
    if (isInteger(str)) BigInt(str)
    else {
      val bd = BigDecimal(str, options.mathContext)
      if (bd.isWhole) bd.toBigInt
      else throw new NumberFormatException(str)
    }
  }

  def readBigDecimal(): BigDecimal =
    parseNumber(BigDecimal(_, options.mathContext))

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
      @tailrec def loop(i: Int): Unit = if (i < result.length) {
        val msb = reader.fromHex(hex.charAt(2 * i))
        val lsb = reader.fromHex(hex.charAt(2 * i + 1))
        result(i) = ((msb << 4) | lsb).toByte
        loop(i + 1)
      }
      loop(0)
      result
    case JsonBinaryFormat.Base64(_, urlSafe) =>
      Base64.decode(checkedValue[String](JsonType.string), urlSafe)
  }

  def readRawJson(): String = {
    skip()
    reader.json.substring(startIdx, endIdx)
  }

  def jsonType: JsonType = reader.jsonType

  override def readMetadata[T](metadata: InputMetadata[T]): Opt[T] = metadata match {
    case JsonType => Opt(reader.jsonType)
    case _ => Opt.Empty
  }

  override def readCustom[T](typeMarker: TypeMarker[T]): Opt[T] =
    typeMarker match {
      case RawJson => readRawJson().opt
      case _ => Opt.Empty
    }

  def readList(): JsonListInput = reader.jsonType match {
    case JsonType.list => new JsonListInput(reader, options, this)
    case _ => expectedError(JsonType.list)
  }

  def readObject(): JsonObjectInput = reader.jsonType match {
    case JsonType.`object` => new JsonObjectInput(reader, options, this)
    case _ => expectedError(JsonType.`object`)
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
  val fieldName: String, reader: JsonReader, options: JsonOptions, callback: AfterElement
) extends JsonStringInput(reader, options, callback) with FieldInput

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

  private[this] var peekIdx = if (end) -1 else reader.index
  private[this] var peekedFields: CrossUtils.NativeDict[Int] = _

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

  private def nextFieldName(): String = {
    reader.skipWs()
    val fieldName = reader.parseString()
    reader.skipWs()
    reader.pass(':')
    fieldName
  }

  def nextField(): JsonStringFieldInput =
    new JsonStringFieldInput(nextFieldName(), reader, options, this)

  def afterElement(): Unit =
    prepareForNext(first = false)

  override def peekField(name: String): Opt[FieldInput] = {
    def peekFieldInput(name: String, idx: Int): JsonStringFieldInput = {
      val forkedReader = new JsonReader(reader.json).setup(_.reset(idx))
      new JsonStringFieldInput(name, forkedReader, options, AfterElementNothing)
    }

    def skipToNextFieldName(): Int = {
      new JsonStringInput(reader).skip()
      reader.skipWs()
      if (reader.isNext('}')) -1
      else {
        reader.pass(',')
        reader.index
      }
    }

    val alreadyPeeked = peekedFields.opt.flatMap(_.getOpt(name).map(idx => peekFieldInput(name, idx)))
    alreadyPeeked orElse {
      val savedIdx = reader.index
      try {
        reader.reset(peekIdx)
        @tailrec def peekRemainingFields(): Opt[FieldInput] =
          if (peekIdx < 0) Opt.Empty
          else {
            val foundName = nextFieldName()
            val valueIndex = reader.index
            if (foundName == name) {
              // intentionally not saving this field into `peekedFields` as a performance optimization for the
              // situation where peeked field is very likely to be the first field (flat sealed hierarchies)
              Opt(peekFieldInput(foundName, valueIndex))
            } else {
              if (peekedFields == null) {
                peekedFields = CrossUtils.newNativeDict[Int]
              }
              peekedFields(foundName) = valueIndex
              peekIdx = skipToNextFieldName()
              peekRemainingFields()
            }
          }
        peekRemainingFields()
      } finally {
        reader.reset(savedIdx)
      }
    }
  }
}

final class JsonReader(val json: String) {
  private[this] var i: Int = 0
  private[this] var value: Any = _
  private[this] var tpe: JsonType = _

  def index: Int = i
  def currentValue: Any = value
  def jsonType: JsonType = tpe

  def reset(idx: Int): Unit = {
    i = idx
  }

  def posInfo(offset: Int): String = {
    @tailrec def loop(idx: Int, line: Int, column: Int): String =
      if (idx >= offset) {
        val lineStart = idx + 1 - column
        val lineEnd = json.indexOf('\n', idx) |> (i => if (i == -1) json.length else i)
        s"(line $line, column $column) (line content: ${json.substring(lineStart, lineEnd)})"
      }
      else if (json.charAt(idx) == '\n') loop(idx + 1, line + 1, 1)
      else loop(idx + 1, line, column + 1)
    loop(0, 1, 1)
  }

  private def currentCharOrEOF =
    if (i < json.length) json.charAt(i).toString else "EOF"

  private def readFailure(msg: String, cause: Throwable = null): Nothing =
    throw new ParseException(s"$msg ${posInfo(i)}", cause)

  @inline private def read(): Char =
    if (i < json.length) {
      val res = json.charAt(i)
      advance()
      res
    } else readFailure("Unexpected EOF")

  @inline def isNext(ch: Char): Boolean =
    i < json.length && json.charAt(i) == ch

  @inline def isNextDigit: Boolean =
    i < json.length && Character.isDigit(json.charAt(i))

  @inline def advance(): Unit =
    i += 1

  def skipWs(): Unit =
    while (i < json.length && Character.isWhitespace(json.charAt(i))) {
      i += 1
    }

  def pass(ch: Char): Unit = {
    val r = read()
    if (r != ch) readFailure(s"'${ch.toChar}' expected, got ${if (r == -1) "EOF" else r.toChar}")
  }

  private def pass(str: String): Unit = {
    var j = 0
    while (j < str.length) {
      if (!isNext(str.charAt(j))) {
        readFailure(s"expected '$str'")
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
    else readFailure(s"Bad hex digit: ${ch.toChar}")

  private def readHex(): Int =
    fromHex(read())

  private def parseNumber(): String = {
    val start = i

    if (isNext('-')) {
      advance()
    }

    def parseDigits(): Unit = {
      if (!isNextDigit) {
        readFailure(s"Expected digit, got $currentCharOrEOF")
      }
      while (isNextDigit) {
        advance()
      }
    }

    if (isNext('0')) {
      advance()
    } else if (isNextDigit) {
      parseDigits()
    } else readFailure(s"Expected '-' or digit, got $currentCharOrEOF")

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
            case '/' => '/'
            case 'b' => '\b'
            case 'f' => '\f'
            case 'n' => '\n'
            case 'r' => '\r'
            case 't' => '\t'
            case 'u' => ((readHex() << 12) + (readHex() << 8) + (readHex() << 4) + readHex()).toChar
            case c => readFailure(s"Unexpected character: '${c.toChar}'")
          }
          sb.append(unesc)
          plainStart = i
        case _ =>
      }
    }
    if (sb != null) {
      sb.append(json, plainStart, i - 1).toString
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
      case c => readFailure(s"Unexpected character: '${c.toChar}'")
    } else readFailure("Unexpected EOF")
    startIndex
  }
}
