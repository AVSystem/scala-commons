package com.avsystem.commons
package jetty.rpc

import java.util.Base64

import com.avsystem.commons.serialization.GenCodec.{ReadFailure, WriteFailure}
import com.avsystem.commons.serialization.{FieldInput, InputType, ListInput, ObjectInput, _}
import jawn.ast.{JArray, JBool, JNull, JNum, JObject, JString, JValue}

object JValueOutput {
  def write[T: GenCodec](value: T): JValue = {
    var result: Opt[JValue] = Opt.Empty
    val output = new JValueOutput(value => result = Opt(value))
    GenCodec.write(output, value)
    result.getOrElse(throw new WriteFailure("No value written by GenCodec"))
  }
}

final class JValueOutput(private val consumer: JValue => Unit) extends AnyVal with Output {
  def writeNull(): Unit = consumer(JNull)
  def writeBoolean(boolean: Boolean): Unit = consumer(JBool(boolean))
  def writeString(str: String): Unit = consumer(JString(str))
  def writeInt(int: Int): Unit = consumer(JNum(int))
  def writeLong(long: Long): Unit = consumer(JNum(long))
  def writeDouble(double: Double): Unit = consumer(JNum(double))
  def writeBinary(binary: Array[Byte]): Unit = consumer(JString(Base64.getEncoder.encodeToString(binary)))
  def writeList() = new JValueListOutput(consumer)
  def writeObject() = new JValueObjectOutput(consumer)
}

final class JValueListOutput(consumer: JArray => Unit) extends ListOutput {
  private val builder = Array.newBuilder[JValue]

  def writeElement(): JValueOutput = new JValueOutput(builder += _)
  def finish(): Unit = consumer(JArray(builder.result()))
}

final class JValueObjectOutput(consumer: JObject => Unit) extends ObjectOutput {
  private val values = MLinkedHashMap.empty[String, JValue]

  def writeField(key: String): JValueOutput = new JValueOutput(v => values += ((key, v)))
  def finish(): Unit = consumer(JObject(values))
}

object JValueInput {
  def read[T: GenCodec](value: JValue): T =
    GenCodec.read[T](new JValueInput(value))
}

class JValueInput(value: JValue) extends Input {

  def inputType: InputType = value match {
    case _: JArray => InputType.List
    case _: JObject => InputType.Object
    case JNull => InputType.Null
    case _ => InputType.Simple
  }

  private def expectedError(expected: String) =
    throw new ReadFailure(s"Expected JSON $expected but got ${value.valueType}: $value")

  def readNull(): Null = value match {
    case JNull => null
    case _ => expectedError("null")
  }
  def readBoolean(): Boolean = value match {
    case v: JBool => v.asBoolean
    case _ => expectedError("boolean")
  }
  def readString(): String = value match {
    case JString(s) => s
    case _ => expectedError("string")
  }
  def readBinary(): Array[Byte] = value match {
    case JString(s) => try Base64.getDecoder.decode(s) catch {
      case e: IllegalArgumentException => throw new ReadFailure(s"Error decoding $s as Base64", e)
    }
    case _ => expectedError("string")
  }
  def readInt(): Int = value match {
    case num: JNum => num.asInt
    case _ => expectedError("number")
  }
  def readLong(): Long = value match {
    case num: JNum => num.asLong
    case _ => expectedError("number")
  }
  def readDouble(): Double = value match {
    case num: JNum => num.asDouble
    case _ => expectedError("number")
  }
  def readList(): JValueListInput = value match {
    case JArray(values) => new JValueListInput(values.iterator)
    case _ => expectedError("array")
  }
  def readObject(): JValueObjectInput = value match {
    case JObject(values) => new JValueObjectInput(values.iterator)
    case _ => expectedError("object")
  }
  def skip(): Unit = ()
}

final class JValueFieldInput(val fieldName: String, value: JValue) extends JValueInput(value) with FieldInput

final class JValueListInput(private val it: Iterator[JValue]) extends AnyVal with ListInput {
  def hasNext: Boolean = it.hasNext
  def nextElement(): JValueInput = new JValueInput(it.next())
}

final class JValueObjectInput(private val it: Iterator[(String, JValue)]) extends AnyVal with ObjectInput {
  def hasNext: Boolean = it.hasNext
  def nextField(): JValueFieldInput = it.next() match {
    case (k, v) => new JValueFieldInput(k, v)
  }
}
