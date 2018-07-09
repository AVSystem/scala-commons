package com.avsystem.commons
package serialization.json

import com.avsystem.commons.serialization.{GenCodec, IsoInstant, ListOutput, ObjectOutput, Output}

object JsonStringOutput {
  def write[T: GenCodec](value: T, options: JsonOptions = JsonOptions.Default): String = {
    val sb = new JStringBuilder
    GenCodec.write[T](new JsonStringOutput(sb, options), value)
    sb.toString
  }
}

trait BaseJsonOutput {
  protected final def writeJsonString(builder: JStringBuilder, str: String, ascii: Boolean): Unit = {
    builder.append('"')
    var i = 0
    var s = 0
    while (i < str.length) {
      val esc = str.charAt(i) match {
        case '"' => "\\\""
        case '\\' => "\\\\"
        case '\b' => "\\b"
        case '\f' => "\\f"
        case '\n' => "\\n"
        case '\r' => "\\r"
        case '\t' => "\\t"
        case c if (ascii && c.toInt > 127) || Character.isISOControl(c) =>
          c.toInt.formatted("\\u%04x")
        case _ => null
      }
      if (esc != null) {
        builder.append(str, s, i).append(esc)
        s = i + 1
      }
      i += 1
    }
    builder.append(str, s, str.length)
    builder.append('"')
  }
}

final class JsonStringOutput(builder: JStringBuilder, options: JsonOptions = JsonOptions.Default)
  extends BaseJsonOutput with Output {

  def writeNull(): Unit = builder.append("null")
  def writeString(str: String): Unit = writeJsonString(builder, str, options.asciiOutput)
  def writeBoolean(boolean: Boolean): Unit = builder.append(boolean.toString)
  def writeInt(int: Int): Unit = builder.append(int.toString)

  private def writeSafeString(str: String): Unit =
    builder.append("\"").append(str).append("\"")

  private def isValidDouble(long: Long): Boolean = long == 0 || {
    import java.lang.{Long => JLong}
    val bitlen = JLong.SIZE - JLong.numberOfLeadingZeros(long) - JLong.numberOfTrailingZeros(long)
    bitlen <= 53 // will it fit into Double mantissa?
  }

  def writeLong(long: Long): Unit =
    if (options.bigNumbers || isValidDouble(long)) builder.append(long.toString)
    else writeSafeString(long.toString)

  def writeDouble(double: Double): Unit =
    if (double.isNaN || double.isInfinity)
      writeSafeString(double.toString)
    else builder.append(double.toString)

  def writeBigInt(bigInt: BigInt): Unit =
    if (options.bigNumbers || bigInt.isValidDouble) builder.append(bigInt.toString)
    else writeSafeString(bigInt.toString)

  private def isValidDouble(bigDecimal: BigDecimal): Boolean = {
    val d = bigDecimal.toDouble
    !d.isInfinity && bigDecimal.equals(BigDecimal.decimal(d, options.mathContext))
  }

  def writeBigDecimal(bigDecimal: BigDecimal): Unit = {
    if (options.bigNumbers || isValidDouble(bigDecimal)) builder.append(bigDecimal.toString)
    else writeSafeString(bigDecimal.toString)
  }

  override def writeTimestamp(millis: Long): Unit = options.dateFormat match {
    case JsonDateFormat.EpochMillis => writeLong(millis)
    case JsonDateFormat.IsoInstant => writeString(IsoInstant.format(millis))
  }

  def writeBinary(binary: Array[Byte]): Unit = options.binaryFormat match {
    case JsonBinaryFormat.ByteArray =>
      val lo = writeList()
      var i = 0
      while (i < binary.length) {
        lo.writeElement().writeByte(binary(i))
        i += 1
      }
      lo.finish()
    case JsonBinaryFormat.HexString =>
      builder.append('"')
      var i = 0
      while (i < binary.length) {
        builder.append(f"${binary(i) & 0xff}%02x") //JS has signed chars
        i += 1
      }
      builder.append('"')
  }

  def writeRawJson(json: String): Unit = builder.append(json)
  def writeList(): JsonListOutput = new JsonListOutput(builder, options)
  def writeObject(): JsonObjectOutput = new JsonObjectOutput(builder, options)
}

final class JsonListOutput(builder: JStringBuilder, options: JsonOptions) extends ListOutput {
  private[this] var first = true
  def writeElement(): JsonStringOutput = {
    builder.append(if (first) '[' else ',')
    first = false
    new JsonStringOutput(builder, options)
  }
  def finish(): Unit = {
    if (first) {
      builder.append('[')
    }
    builder.append(']')
  }
}

final class JsonObjectOutput(builder: JStringBuilder, options: JsonOptions)
  extends BaseJsonOutput with ObjectOutput {

  private[this] var first = true
  def writeField(key: String): JsonStringOutput = {
    builder.append(if (first) '{' else ',')
    first = false
    writeJsonString(builder, key, options.asciiOutput)
    builder.append(':')
    new JsonStringOutput(builder, options)
  }
  def finish(): Unit = {
    if (first) {
      builder.append('{')
    }
    builder.append('}')
  }
}
