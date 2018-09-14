package com.avsystem.commons
package serialization.json

import com.avsystem.commons.serialization.{GenCodec, IsoInstant, ListOutput, ObjectOutput, Output}

object JsonStringOutput {
  def write[T: GenCodec](value: T, options: JsonOptions = JsonOptions.Default): String = {
    val sb = new JStringBuilder
    GenCodec.write[T](new JsonStringOutput(sb, options), value)
    sb.toString
  }

  def writePretty[T: GenCodec](value: T): String =
    write[T](value, JsonOptions.Pretty)
}

trait BaseJsonOutput {
  protected final def indent(builder: JStringBuilder, indentSize: OptArg[Int], depth: Int): Unit =
    indentSize match {
      case OptArg(size) => builder.append('\n').append(" " * (depth * size))
      case OptArg.Empty =>
    }

  protected final def writeJsonString(builder: JStringBuilder, str: String, ascii: Boolean): Unit = {
    builder.append('"')
    var i = 0
    var s = 0
    while (i < str.length) {
      val ch = str.charAt(i)
      val esc = ch match {
        case '"' => '"'
        case '\\' => '\\'
        case '\b' => 'b'
        case '\f' => 'f'
        case '\n' => 'n'
        case '\r' => 'r'
        case '\t' => 't'
        case _ => (if ((ascii && ch.toInt > 127) || Character.isISOControl(ch)) 1 else 0).toChar
      }
      if (esc != 0) {
        builder.append(str, s, i).append('\\')
        s = i + 1
        if (esc != 1) {
          builder.append(esc)
        } else {
          builder.append('u').append(toHex((ch >> 12) & 15)).append(toHex((ch >> 8) & 15))
            .append(toHex((ch >> 4) & 15)).append(toHex(ch & 15))
        }
      }
      i += 1
    }
    builder.append(str, s, str.length).append('"')
  }

  protected final def toHex(nibble: Int): Char = (nibble + (if (nibble >= 10) 'a' - 10 else '0')).toChar
}

final class JsonStringOutput(builder: JStringBuilder, options: JsonOptions = JsonOptions.Default, depth: Int = 0)
  extends BaseJsonOutput with Output {

  def writeNull(): Unit = builder.append("null")
  def writeString(str: String): Unit = writeJsonString(builder, str, options.asciiOutput)
  def writeBoolean(boolean: Boolean): Unit = builder.append(boolean)
  def writeInt(int: Int): Unit = builder.append(int)
  def writeLong(long: Long): Unit = builder.append(long)

  def writeDouble(double: Double): Unit =
    if (double.isNaN || double.isInfinity)
      builder.append('"').append(double).append('"')
    else builder.append(double)

  def writeBigInt(bigInt: BigInt): Unit = builder.append(bigInt)
  def writeBigDecimal(bigDecimal: BigDecimal): Unit = builder.append(bigDecimal)

  override def writeTimestamp(millis: Long): Unit = options.dateFormat match {
    case JsonDateFormat.EpochMillis => writeLong(millis)
    case JsonDateFormat.IsoInstant => builder.append('"').append(IsoInstant.format(millis)).append('"')
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
    indent(builder, options.formatting.indentSize, depth)
    first = false
    new JsonStringOutput(builder, options, depth)
  }

  def finish(): Unit = {
    if (first) {
      builder.append('[')
    } else {
      indent(builder, options.formatting.indentSize, depth - 1)
    }
    builder.append(']')
  }
}

final class JsonObjectOutput(builder: JStringBuilder, options: JsonOptions, depth: Int)
  extends BaseJsonOutput with ObjectOutput {

  private[this] var first = true

  def writeField(key: String): JsonStringOutput = {
    builder.append(if (first) '{' else ',')
    indent(builder, options.formatting.indentSize, depth)
    first = false
    writeJsonString(builder, key, options.asciiOutput)
    builder.append(':').append(" " * options.formatting.afterColon)
    new JsonStringOutput(builder, options, depth)
  }

  def finish(): Unit = {
    if (first) {
      builder.append('{')
    } else {
      indent(builder, options.formatting.indentSize, depth - 1)
    }
    builder.append('}')
  }
}
