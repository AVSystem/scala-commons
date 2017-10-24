package com.avsystem.commons
package serialization.json

import java.io.{StringWriter, Writer}

import com.avsystem.commons.serialization.{GenCodec, ListOutput, ObjectOutput, Output}

object JsonStringOutput {
  def write[T: GenCodec](value: T): String = {
    val sw = new StringWriter
    GenCodec.write[T](new JsonStringOutput(sw), value)
    sw.toString
  }
}

abstract class BaseJsonOutput {
  protected final def writeJsonString(writer: Writer, str: String): Unit = {
    writer.write('"')
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
        writer.write(str, s, i - s)
        writer.write(esc)
        s = i + 1
      }
      i += 1
    }
    writer.write(str, s, str.length - s)
    writer.write('"')
  }
}

final class JsonStringOutput(writer: Writer) extends BaseJsonOutput with Output {
  def writeNull(): Unit = writer.write("null")
  def writeString(str: String): Unit = writeJsonString(writer, str)
  def writeBoolean(boolean: Boolean): Unit = writer.write(boolean.toString)
  def writeInt(int: Int): Unit = writer.write(int.toString)
  def writeLong(long: Long): Unit = {
    val asInt = long.toInt
    if (asInt == long) writeInt(asInt)
    else writeString(long.toString)
  }
  def writeDouble(double: Double): Unit = writer.write(double.toString)
  def writeBinary(binary: Array[Byte]): Unit = {
    writer.write('"')
    var i = 0
    while (i < binary.length) {
      writer.write(f"${binary(i)}%02x")
      i += 1
    }
    writer.write('"')
  }
  def writeList(): ListOutput = new JsonListOutput(writer)
  def writeObject(): ObjectOutput = new JsonObjectOutput(writer)
}

final class JsonListOutput(writer: Writer) extends ListOutput {
  private[this] var first = true
  def writeElement(): Output = {
    writer.write(if (first) '[' else ',')
    first = false
    new JsonStringOutput(writer)
  }
  def finish(): Unit = {
    if (first) {
      writer.write('[')
    }
    writer.write(']')
  }
}

final class JsonObjectOutput(writer: Writer) extends BaseJsonOutput with ObjectOutput {
  private[this] var first = true
  def writeField(key: String): Output = {
    writer.write(if (first) '{' else ',')
    first = false
    writeJsonString(writer, key)
    writer.write(':')
    new JsonStringOutput(writer)
  }
  def finish(): Unit = {
    if (first) {
      writer.write('{')
    }
    writer.write('}')
  }
}
