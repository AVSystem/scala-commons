package com.avsystem.commons
package rpc.akka.serialization

import akka.util.{ByteString, ByteStringBuilder}
import com.avsystem.commons.serialization.{ListOutput, ObjectOutput, Output}

/**
  * @author Wojciech Milewski
  */
private[akka] class ByteStringLinearOutput(builder: ByteStringBuilder) extends Output {

  override def writeNull(): Unit = WriteOps.writeNull(builder)
  override def writeString(str: String): Unit = WriteOps.writeString(str)(builder)
  override def writeDouble(double: Double): Unit = WriteOps.writeDouble(double)(builder)
  override def writeBinary(binary: Array[Byte]): Unit = WriteOps.writeBinary(binary)(builder)
  override def writeInt(int: Int): Unit = WriteOps.writeInt(int)(builder)
  override def writeList(): ListOutput = new ByteStringLinearListOutput(builder)
  override def writeBoolean(boolean: Boolean): Unit = WriteOps.writeBoolean(boolean)(builder)
  override def writeObject(): ObjectOutput = new ByteStringLinearObjectOutput(builder)
  override def writeLong(long: Long): Unit = WriteOps.writeLong(long)(builder)

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