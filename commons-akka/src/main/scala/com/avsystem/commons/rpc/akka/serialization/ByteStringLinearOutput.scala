package com.avsystem.commons
package rpc.akka.serialization

import java.nio.charset.StandardCharsets

import akka.util.{ByteString, ByteStringBuilder}
import com.avsystem.commons.serialization.{ListOutput, ObjectOutput, Output}

/**
  * @author Wojciech Milewski
  */
final class ByteStringLinearOutput(private val builder: ByteStringBuilder) extends AnyVal with Output {
  import ByteOrderImplicits._

  override def writeNull(): Unit = {
    builder += NullMarker.byte
  }
  override def writeString(str: String): Unit = {
    val strBytes = str.getBytes(StandardCharsets.UTF_8)
    builder += StringMarker.byte
    builder.putInt(strBytes.length)
    builder.putBytes(strBytes)
  }
  override def writeBinary(binary: Array[Byte]): Unit = {
    builder += ByteArrayMarker.byte
    builder.putInt(binary.length)
    builder.putBytes(binary)
  }
  override def writeDouble(double: Double): Unit = writeSinglePrimitive(DoubleMarker, _.putDouble(double))
  override def writeInt(int: Int): Unit = writeSinglePrimitive(IntMarker, _.putInt(int))
  override def writeLong(long: Long): Unit = writeSinglePrimitive(LongMarker, _.putLong(long))
  override def writeBoolean(boolean: Boolean): Unit = {
    builder += BooleanMarker.byte
    builder += (if (boolean) TrueByte else FalseByte)
  }
  override def writeList(): ListOutput = {
    builder += ListStartMarker.byte
    new ByteStringLinearListOutput(builder)
  }
  override def writeObject(): ObjectOutput = {
    builder += ObjectStartMarker.byte
    new ByteStringLinearObjectOutput(builder)
  }

  private def writeSinglePrimitive(marker: Marker, f: ByteStringBuilder => Unit): Unit = {
    builder += marker.byte
    f(builder)
  }

  def result: ByteString = builder.result()
}

private class ByteStringLinearListOutput(private val builder: ByteStringBuilder) extends AnyVal with ListOutput {
  override def writeElement(): Output = new ByteStringLinearOutput(builder)
  override def finish(): Unit = {
    builder += ListEndMarker.byte
  }
}

private class ByteStringLinearObjectOutput(private val builder: ByteStringBuilder) extends AnyVal with ObjectOutput {

  override def writeField(key: String): Output = {
    WriteOps.writeString(key)(builder)
    new ByteStringLinearOutput(builder)
  }
  override def finish(): Unit = {
    builder += ObjectEndMarker.byte
  }
}