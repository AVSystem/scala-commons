package com.avsystem.commons
package jetty.rpc

import java.util.Base64

import com.avsystem.commons.serialization.GenCodec.{ReadFailure, WriteFailure}
import com.avsystem.commons.serialization.{FieldInput, InputType, ListInput, ObjectInput, _}
import jawn.ast.{JArray, JBool, JFalse, JNull, JNum, JObject, JString, JTrue, JValue}

object JValueOutput {
  def write[T: GenCodec](value: T): JValue = {
    var result: Opt[JValue] = Opt.Empty
    val output = new JValueOutput(value => result = Opt(value))
    GenCodec.write(output, value)
    result.getOrElse(throw new WriteFailure("No value written by GenCodec"))
  }
}

final class JValueOutput(private val consumer: JValue => Unit) extends AnyVal with Output {
  def writeNull() = consumer(JNull)
  def writeBoolean(boolean: Boolean) = consumer(if (boolean) JTrue else JFalse)
  def writeString(str: String) = consumer(JString(str))
  def writeInt(int: Int) = consumer(JNum(int))
  def writeLong(long: Long) = consumer(JNum(long))
  def writeDouble(double: Double) = consumer(JNum(double))
  def writeBinary(binary: Array[Byte]) = consumer(JString(Base64.getEncoder.encodeToString(binary)))
  def writeList() = new JValueListOutput(consumer)
  def writeObject() = new JValueObjectOutput(consumer)
}

final class JValueListOutput(consumer: JArray => Unit) extends ListOutput {
  private val builder = Array.newBuilder[JValue]

  def writeElement() = new JValueOutput(builder += _)
  def finish() = consumer(JArray(builder.result()))
}

final class JValueObjectOutput(consumer: JObject => Unit) extends ObjectOutput {
  private val builder = MMap.newBuilder[String, JValue]

  def writeField(key: String) = new JValueOutput(v => builder += ((key, v)))
  def finish() = consumer(JObject(builder.result()))
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
    throw new ReadFailure(s"Expected JSON $expected but got ${value.valueType}")

  def readNull() = value match {
    case JNull => null
    case _ => expectedError("null")
  }
  def readBoolean() = value match {
    case v: JBool => v.asBoolean
    case _ => expectedError("boolean")
  }
  def readString() = value match {
    case JString(s) => s
    case _ => expectedError("string")
  }
  def readBinary() = value match {
    case JString(s) => try Base64.getDecoder.decode(s) catch {
      case e: IllegalArgumentException => throw new ReadFailure("error decoding Base64: " + e.getMessage, e)
    }
    case _ => expectedError("string")
  }
  def readInt() = value match {
    case num: JNum => Try(num.asInt).getOrElse(throw new ReadFailure(s"not an int: $num"))
    case _ => expectedError("number")
  }
  def readLong() = value match {
    case num: JNum => Try(num.asLong).getOrElse(throw new ReadFailure(s"not a long: $num"))
    case _ => expectedError("string")
  }
  def readDouble() = value match {
    case num: JNum => num.asDouble
    case _ => expectedError("number")
  }
  def readList() = value match {
    case JArray(values) => new JsValueListInput(values.iterator)
    case _ => expectedError("array")
  }
  def readObject() = value match {
    case JObject(values) => new JsValueObjectInput(values.iterator)
    case _ => expectedError("object")
  }
  def skip() = ()
}

final class JValueFieldInput(val fieldName: String, value: JValue) extends JValueInput(value) with FieldInput

final class JsValueListInput(private val it: Iterator[JValue]) extends AnyVal with ListInput {
  def hasNext = it.hasNext
  def nextElement() = new JValueInput(it.next())
}

final class JsValueObjectInput(private val it: Iterator[(String, JValue)]) extends AnyVal with ObjectInput {
  def hasNext = it.hasNext
  def nextField() = it.next() match {
    case (k, v) => new JValueFieldInput(k, v)
  }
}
