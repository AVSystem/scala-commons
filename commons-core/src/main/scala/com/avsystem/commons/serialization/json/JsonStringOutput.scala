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
  protected final def indent(builder: JStringBuilder, options: JsonOptions, depth: Int): Unit =
    options.indentSize match {
      case OptArg(size) => builder.append('\n').append(" " * (depth * size))
      case OptArg.Empty =>
    }

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

final class JsonStringOutput(builder: JStringBuilder, options: JsonOptions = JsonOptions.Default, depth: Int = 0)
  extends BaseJsonOutput with Output {

  def writeNull(): Unit = builder.append("null")
  def writeString(str: String): Unit = writeJsonString(builder, str, options.asciiOutput)
  def writeBoolean(boolean: Boolean): Unit = builder.append(boolean.toString)
  def writeInt(int: Int): Unit = builder.append(int.toString)
  def writeLong(long: Long): Unit = builder.append(long.toString)

  def writeDouble(double: Double): Unit =
    if (double.isNaN || double.isInfinity)
      builder.append("\"").append(double.toString).append("\"")
    else builder.append(double.toString)

  def writeBigInt(bigInt: BigInt): Unit = builder.append(bigInt.toString)
  def writeBigDecimal(bigDecimal: BigDecimal): Unit = builder.append(bigDecimal.toString)

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
  def writeList(): JsonListOutput = new JsonListOutput(builder, options, depth + 1)
  def writeObject(): JsonObjectOutput = new JsonObjectOutput(builder, options, depth + 1)
}

final class JsonListOutput(builder: JStringBuilder, options: JsonOptions, depth: Int)
  extends BaseJsonOutput with ListOutput {

  private[this] var first = true
  def writeElement(): JsonStringOutput = {
    builder.append(if (first) '[' else ',')
    indent(builder, options, depth)
    first = false
    new JsonStringOutput(builder, options, depth)
  }
  def finish(): Unit = {
    if (first) {
      builder.append('[')
    } else {
      indent(builder, options, depth - 1)
    }
    builder.append(']')
  }
}

final class JsonObjectOutput(builder: JStringBuilder, options: JsonOptions, depth: Int)
  extends BaseJsonOutput with ObjectOutput {

  private[this] var first = true
  def writeField(key: String): JsonStringOutput = {
    builder.append(if (first) '{' else ',')
    indent(builder, options, depth)
    first = false
    writeJsonString(builder, key, options.asciiOutput)
    builder.append(':')
    new JsonStringOutput(builder, options, depth)
  }
  def finish(): Unit = {
    if (first) {
      builder.append('{')
    } else {
      indent(builder, options, depth - 1)
    }
    builder.append('}')
  }
}
