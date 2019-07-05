package com.avsystem.commons
package serialization.json

import com.avsystem.commons.serialization._

import scala.annotation.tailrec

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
      case OptArg.Empty =>
      case OptArg(size) =>
        builder.append('\n')
        writeSpaces(builder, depth * size)
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
          builder.append('u').append(toHex((ch >> 12) & 0xF)).append(toHex((ch >> 8) & 0xF))
            .append(toHex((ch >> 4) & 0xF)).append(toHex(ch & 0xF))
        }
      }
      i += 1
    }
    builder.append(str, s, str.length).append('"')
  }

  @tailrec protected final def writeSpaces(builder: JStringBuilder, n: Int): Unit =
    if (n > 0) {
      builder.append(' ')
      writeSpaces(builder, n - 1)
    }

  protected final def toHex(nibble: Int): Char =
    (nibble + (if (nibble >= 10) 'a' - 10 else '0')).toChar
}

final class JsonStringOutput(builder: JStringBuilder, options: JsonOptions = JsonOptions.Default, depth: Int = 0)
  extends BaseJsonOutput with OutputAndSimpleOutput {

  def writeNull(): Unit = builder.append("null")
  def writeString(str: String): Unit = writeJsonString(builder, str, options.asciiOutput)
  def writeBoolean(boolean: Boolean): Unit = builder.append(boolean)
  def writeInt(int: Int): Unit = builder.append(int)
  def writeLong(long: Long): Unit = builder.append(long)

  // Required by SenML spec (RFC8428), should be configurable in next binary incompatible version
  private def lowe(str: String): String = str.replace('E', 'e')

  override def writeFloat(float: Float): Unit =
    if (java.lang.Float.isFinite(float)) builder.append(lowe(float.toString))
    else builder.append('"').append(lowe(float.toString)).append('"')

  def writeDouble(double: Double): Unit =
    if (java.lang.Double.isFinite(double)) builder.append(lowe(double.toString))
    else builder.append('"').append(lowe(double.toString)).append('"')

  def writeBigInt(bigInt: BigInt): Unit = builder.append(bigInt)
  def writeBigDecimal(bigDecimal: BigDecimal): Unit = builder.append(lowe(bigDecimal.toString))

  override def writeTimestamp(millis: Long): Unit = options.dateFormat match {
    case JsonDateFormat.EpochMillis =>
      writeLong(millis)
    case JsonDateFormat.IsoInstant =>
      builder.append('"').append(IsoInstant.format(millis)).append('"')
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
        val b = binary(i)
        builder.append(toHex((b >> 4) & 0xF)).append(toHex(b & 0xF))
        i += 1
      }
      builder.append('"')
    case JsonBinaryFormat.Base64(withoutPadding, urlSafe) =>
      builder.append('"').append(Base64.encode(binary, withoutPadding, urlSafe)).append('"')
  }

  def writeRawJson(json: String): Unit = builder.append(json)

  override def keepsMetadata(metadata: InputMetadata[_]): Boolean =
    metadata == JsonType

  override def writeCustom[T](typeMarker: TypeMarker[T], value: T): Boolean =
    typeMarker match {
      case RawJson => writeRawJson(value); true
      case _ => false
    }

  def writeList(): JsonListOutput = new JsonListOutput(builder, options, depth + 1)
  def writeObject(): JsonObjectOutput = new JsonObjectOutput(builder, options, depth + 1)
}

final class JsonListOutput(builder: JStringBuilder, options: JsonOptions, depth: Int)
  extends BaseJsonOutput with ListOutput {

  override def sizePolicy: SizePolicy = SizePolicy.Ignored

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

  override def sizePolicy: SizePolicy = SizePolicy.Ignored

  private[this] var first = true

  def writeField(key: String): JsonStringOutput = {
    builder.append(if (first) '{' else ',')
    indent(builder, options.formatting.indentSize, depth)
    first = false
    writeJsonString(builder, key, options.asciiOutput)
    builder.append(':')
    writeSpaces(builder, options.formatting.afterColon)
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
