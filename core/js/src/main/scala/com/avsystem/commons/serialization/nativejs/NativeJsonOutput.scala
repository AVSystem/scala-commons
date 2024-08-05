package com.avsystem.commons
package serialization.nativejs

import com.avsystem.commons.serialization._
import com.avsystem.commons.serialization.json.RawJson

import scala.scalajs.js
import scala.scalajs.js.JSON

final class NativeJsonOutput(
  valueConsumer: js.Any => Unit,
  options: NativeFormatOptions,
) extends OutputAndSimpleOutput {

  override def writeNull(): Unit =
    valueConsumer(null)

  override def writeString(str: String): Unit =
    valueConsumer(str)

  override def writeDouble(double: Double): Unit =
    valueConsumer(double)

  override def writeInt(int: Int): Unit =
    valueConsumer(int)

  override def writeLong(long: Long): Unit = options.longFormat match {
    case NativeLongFormat.RawString => writeString(long.toString)
    case NativeLongFormat.JsNumber => writeDouble(long.toDouble)
    case NativeLongFormat.JsBigInt => writeRaw(js.BigInt(long.toString))
  }

  override def writeBigInt(bigInt: BigInt): Unit = options.bigIntFormat match {
    case NativeBigIntFormat.RawString => writeString(bigInt.toString)
    case NativeBigIntFormat.JsBigInt => writeRaw(js.BigInt(bigInt.toString))
  }

  override def writeBigDecimal(bigDecimal: BigDecimal): Unit =
    writeString(bigDecimal.toString)

  override def writeBoolean(boolean: Boolean): Unit =
    valueConsumer(boolean)

  override def writeList(): ListOutput =
    new NativeJsonListOutput(valueConsumer, options)

  override def writeObject(): ObjectOutput =
    new NativeJsonObjectOutput(valueConsumer, options)

  override def writeBinary(binary: Array[Byte]): Unit = {
    import js.JSConverters._
    valueConsumer(binary.toJSArray)
  }

  override def writeTimestamp(millis: Long): Unit = options.dateFormat match {
    case NativeDateFormat.RawString => writeString(millis.toString)
    case NativeDateFormat.JsNumber => writeDouble(millis.toDouble)
    case NativeDateFormat.JsDate => writeRaw(new js.Date(millis.toDouble))
  }

  override def writeCustom[T](typeMarker: TypeMarker[T], value: T): Boolean =
    typeMarker match {
      case RawJson => writeRaw(JSON.parse(value)); true
      case _ => false
    }

  def writeRaw(raw: js.Any): Unit = valueConsumer(raw)
}

final class NativeJsonListOutput(
  valueConsumer: js.Any => Unit,
  options: NativeFormatOptions,
) extends ListOutput {
  private val builder = new js.Array[js.Any]()

  override def writeElement(): Output = new NativeJsonOutput(el => builder.append(el), options)
  override def finish(): Unit = valueConsumer(builder)
}

final class NativeJsonObjectOutput(
  valueConsumer: js.Any => Unit,
  options: NativeFormatOptions,
) extends ObjectOutput {
  private val builder = js.Dictionary.empty[js.Any]

  override def writeField(key: String): Output = new NativeJsonOutput(el => builder(key) = el, options)
  override def finish(): Unit = valueConsumer(builder)
}

object NativeJsonOutput {
  def write[T: GenCodec](value: T, options: NativeFormatOptions = NativeFormatOptions.RawString): js.Any = {
    var result: js.Any = null
    GenCodec.write(new NativeJsonOutput(value => result = value, options), value)
    result
  }

  def writeAsString[T: GenCodec](value: T, options: NativeFormatOptions = NativeFormatOptions.RawString): String =
    JSON.stringify(write(value, options))
}
