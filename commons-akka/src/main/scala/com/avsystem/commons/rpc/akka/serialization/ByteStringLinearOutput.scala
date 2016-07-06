package com.avsystem.commons
package rpc.akka.serialization

import akka.util.{ByteString, ByteStringBuilder}
import com.avsystem.commons.serialization.{ListOutput, ObjectOutput, Output}

/**
  * @author Wojciech Milewski
  */
private[akka] class ByteStringLinearOutput(builder: ByteStringBuilder) extends Output {

  private var resultCache: Option[ByteString] = None

  override def writeNull(): Unit = atomic(WriteOps.writeNull(builder))
  override def writeString(str: String): Unit = atomic {
    WriteOps.writeString(str)(builder)
  }
  override def writeDouble(double: Double): Unit = atomic(WriteOps.writeDouble(double)(builder))
  override def writeBinary(binary: Array[Byte]): Unit = atomic(WriteOps.writeBinary(binary)(builder))
  override def writeInt(int: Int): Unit = atomic(WriteOps.writeInt(int)(builder))
  override def writeList(): ListOutput = new ByteStringLinearListOutput(builder, () => resultCache = Some(builder.result))
  override def writeBoolean(boolean: Boolean): Unit = atomic(WriteOps.writeBoolean(boolean)(builder))
  override def writeObject(): ObjectOutput = new ByteStringLinearObjectOutput(builder, () => resultCache = Some(builder.result))
  override def writeLong(long: Long): Unit = atomic(WriteOps.writeLong(long)(builder))

  private def atomic(code: => Any): Unit = {
    code
    resultCache = Some(builder.result)
  }

  def result: ByteString = resultCache.getOrElse(throw new IllegalStateException("No data written"))
}

private class ByteStringLinearListOutput(builder: ByteStringBuilder, onFinish: () => Unit) extends ListOutput {
  builder += ListStartMarker.byte

  override def writeElement(): Output = new ByteStringLinearOutput(builder)
  override def finish(): Unit = {
    builder += ListEndMarker.byte
    onFinish()
  }
}

private class ByteStringLinearObjectOutput(builder: ByteStringBuilder, onFinish: () => Unit) extends ObjectOutput {
  builder += ObjectStartMarker.byte

  override def writeField(key: String): Output = {
    WriteOps.writeString(key)(builder)
    new ByteStringLinearOutput(builder)
  }
  override def finish(): Unit = {
    builder += ObjectEndMarker.byte
    onFinish()
  }
}