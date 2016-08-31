package com.avsystem.commons
package rpc.akka.serialization

import java.nio.charset.StandardCharsets

import akka.util.{ByteString, ByteStringBuilder}
import com.avsystem.commons.serialization.{ListOutput, ObjectOutput, Output}

/**
  * @author Wojciech Milewski
  */
private[akka] class ByteStringLinearOutput(builder: ByteStringBuilder) extends Output {
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
  override def writeList(): ListOutput = new ByteStringLinearListOutput(builder)
  override def writeObject(): ObjectOutput = new ByteStringLinearObjectOutput(builder)

  private def writeSinglePrimitive(marker: Marker, f: ByteStringBuilder => Unit): Unit = {
    builder += marker.byte
    f(builder)
  }

  def result: ByteString = builder.result()
}

private class ByteStringLinearListOutput(builder: ByteStringBuilder) extends ListOutput {
  builder += ListStartMarker.byte

  override def writeElement(): Output = new ByteStringLinearOutput(builder)
  override def finish(): Unit = {
    builder += ListEndMarker.byte
  }
}

private class ByteStringLinearObjectOutput(builder: ByteStringBuilder) extends ObjectOutput {
  builder += ObjectStartMarker.byte

  override def writeField(key: String): Output = {
    WriteOps.writeString(key)(builder)
    new ByteStringLinearOutput(builder)
  }
  override def finish(): Unit = {
    builder += ObjectEndMarker.byte
  }
}