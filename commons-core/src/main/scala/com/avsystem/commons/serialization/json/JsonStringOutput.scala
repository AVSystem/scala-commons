package com.avsystem.commons
package serialization.json

import com.avsystem.commons.serialization.{GenCodec, ListOutput, ObjectOutput, Output}

object JsonStringOutput {
  def write[T: GenCodec](value: T): String = {
    val sb = new JStringBuilder
    GenCodec.write[T](new JsonStringOutput(sb), value)
    sb.toString
  }
}

trait BaseJsonOutput {
  protected final def writeJsonString(builder: JStringBuilder, str: String): Unit = {
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
        case c if Character.isISOControl(c) => c.toInt.formatted("\\u%04x")
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

final class JsonStringOutput(builder: JStringBuilder) extends BaseJsonOutput with Output {
  def writeNull(): Unit = builder.append("null")
  def writeString(str: String): Unit = writeJsonString(builder, str)
  def writeBoolean(boolean: Boolean): Unit = builder.append(boolean.toString)
  def writeInt(int: Int): Unit = builder.append(int.toString)
  def writeLong(long: Long): Unit = {
    val asInt = long.toInt
    if (asInt == long) writeInt(asInt)
    else writeString(long.toString)
  }
  def writeDouble(double: Double): Unit = {
    if (java.lang.Double.isNaN(double) || double == Double.NegativeInfinity || double == Double.PositiveInfinity)
      writeString(double.toString)
    else builder.append(double.toString)
  }
  def writeBinary(binary: Array[Byte]): Unit = {
    builder.append('"')
    var i = 0
    while (i < binary.length) {
      builder.append(f"${binary(i)}%02x")
      i += 1
    }
    builder.append('"')
  }
  def writeRawJson(json: String): Unit = builder.append(json)
  def writeList(): JsonListOutput = new JsonListOutput(builder)
  def writeObject(): JsonObjectOutput = new JsonObjectOutput(builder)
}

final class JsonListOutput(builder: JStringBuilder) extends ListOutput {
  private[this] var first = true
  def writeElement(): JsonStringOutput = {
    builder.append(if (first) '[' else ',')
    first = false
    new JsonStringOutput(builder)
  }
  def finish(): Unit = {
    if (first) {
      builder.append('[')
    }
    builder.append(']')
  }
}

final class JsonObjectOutput(builder: JStringBuilder) extends BaseJsonOutput with ObjectOutput {
  private[this] var first = true
  def writeField(key: String): JsonStringOutput = {
    builder.append(if (first) '{' else ',')
    first = false
    writeJsonString(builder, key)
    builder.append(':')
    new JsonStringOutput(builder)
  }
  def finish(): Unit = {
    if (first) {
      builder.append('{')
    }
    builder.append('}')
  }
}
