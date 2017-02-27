package com.avsystem.commons
package rpc.akka.serialization

import java.nio.charset.StandardCharsets

import akka.util.{ByteString, ByteStringBuilder}
import com.avsystem.commons.rpc.akka.serialization.ByteOrderImplicits._
import com.avsystem.commons.serialization.{ListOutput, ObjectOutput, Output}

/**
  * @author Wojciech Milewski
  */
private[akka] class ByteStringOutput extends Output {

  private var resultCache: Option[ByteString] = None

  override def writeNull(): Unit = atomic(WriteOps.writeNull)

  override def writeString(str: String): Unit = atomic(WriteOps.writeString(str))

  override def writeByte(byte: Byte): Unit = atomic(WriteOps.writeByte(byte))

  override def writeShort(short: Short): Unit = atomic(WriteOps.writeShort(short))

  override def writeInt(int: Int): Unit = atomic(WriteOps.writeInt(int))

  override def writeLong(long: Long): Unit = atomic(WriteOps.writeLong(long))

  override def writeFloat(float: Float): Unit = atomic(WriteOps.writeFloat(float))

  override def writeDouble(double: Double): Unit = atomic(WriteOps.writeDouble(double))

  override def writeBinary(binary: Array[Byte]): Unit = atomic(WriteOps.writeBinary(binary))

  override def writeList(): ListOutput = new DirectByteArrayListOutput(result => assignNewResult(result))

  override def writeBoolean(boolean: Boolean): Unit = atomic(WriteOps.writeBoolean(boolean))

  override def writeObject(): ObjectOutput = new DirectByteStringObjectOutput(result => assignNewResult(result))

  def result: ByteString = resultCache.getOrElse(ByteString.empty)

  private def atomic(code: ByteStringBuilder => Unit): Unit = {
    val builder: ByteStringBuilder = ByteString.newBuilder
    assignNewResult {
      code(builder)
      builder.result()
    }
  }

  private def assignNewResult(result: => ByteString): Unit = {
    if (resultCache.isEmpty) {
      resultCache = Option(result)
    } else {
      throw new IllegalStateException("Data already written!")
    }
  }

}

private object WriteOps {
  def writeNull(builder: ByteStringBuilder): Unit = {
    builder += NullMarker.byte
  }

  def writeString(value: String)(builder: ByteStringBuilder): Unit = {
    val strBytes = value.getBytes(StandardCharsets.UTF_8)
    writeStringContent(strBytes)(builder)
  }

  def writeStringContent(content: Array[Byte])(builder: ByteStringBuilder): Unit = {
    builder += StringMarker.byte
    builder.putInt(content.length)
    builder.putBytes(content)
  }

  def writeByte(value: Byte)(builder: ByteStringBuilder): Unit = {
    builder += ByteMarker.byte
    builder += value
  }

  def writeBinary(value: Array[Byte])(builder: ByteStringBuilder): Unit = {
    builder += ByteArrayMarker.byte
    builder.putInt(value.length)
    builder.putBytes(value)
  }

  def writeBoolean(value: Boolean)(builder: ByteStringBuilder): Unit = {
    builder += BooleanMarker.byte
    builder += (if (value) TrueByte else FalseByte)
  }
  def writeShort(value: Short)(builder: ByteStringBuilder): Unit = writeSinglePrimitive(ShortMarker, _.putShort(value))(builder)
  def writeInt(value: Int)(builder: ByteStringBuilder): Unit = writeSinglePrimitive(IntMarker, _.putInt(value))(builder)
  def writeLong(value: Long)(builder: ByteStringBuilder): Unit = writeSinglePrimitive(LongMarker, _.putLong(value))(builder)
  def writeFloat(value: Float)(builder: ByteStringBuilder): Unit = writeSinglePrimitive(FloatMarker, _.putFloat(value))(builder)
  def writeDouble(value: Double)(builder: ByteStringBuilder): Unit = writeSinglePrimitive(DoubleMarker, _.putDouble(value))(builder)

  private def writeSinglePrimitive(marker: Marker, f: ByteStringBuilder => Unit)(builder: ByteStringBuilder): Unit = {
    builder += marker.byte
    f(builder)
  }
}

private sealed trait DataEntry {
  def write(builder: ByteStringBuilder): Unit
  def contentSize: Int
}
private case object NullData extends DataEntry {
  override def write(builder: ByteStringBuilder): Unit = WriteOps.writeNull(builder)
  override def contentSize: Int = ByteBytes
}
private final case class StringData(value: String) extends DataEntry {
  private val strBytes = value.getBytes(StandardCharsets.UTF_8)

  override def write(builder: ByteStringBuilder): Unit = WriteOps.writeStringContent(strBytes)(builder)
  override def contentSize: Int = ByteBytes + IntBytes + strBytes.length
}
private final case class ByteData(value: Byte) extends DataEntry {
  override def write(builder: ByteStringBuilder): Unit = WriteOps.writeByte(value)(builder)
  override def contentSize: Int = ByteBytes + ByteBytes
}
private final case class ShortData(value: Short) extends DataEntry {
  override def write(builder: ByteStringBuilder): Unit = WriteOps.writeShort(value)(builder)
  override def contentSize: Int = ByteBytes + ShortBytes
}
private final case class IntData(value: Int) extends DataEntry {
  override def write(builder: ByteStringBuilder): Unit = WriteOps.writeInt(value)(builder)
  override def contentSize: Int = ByteBytes + IntBytes
}
private final case class LongData(value: Long) extends DataEntry {
  override def write(builder: ByteStringBuilder): Unit = WriteOps.writeLong(value)(builder)
  override def contentSize: Int = ByteBytes + LongBytes
}
private final case class FloatData(value: Float) extends DataEntry {
  override def write(builder: ByteStringBuilder): Unit = WriteOps.writeFloat(value)(builder)
  override def contentSize: Int = ByteBytes + FloatBytes
}
private final case class DoubleData(value: Double) extends DataEntry {
  override def write(builder: ByteStringBuilder): Unit = WriteOps.writeDouble(value)(builder)
  override def contentSize: Int = ByteBytes + DoubleBytes
}
private final case class ByteArrayData(value: Array[Byte]) extends DataEntry {
  override def write(builder: ByteStringBuilder): Unit = WriteOps.writeBinary(value)(builder)
  override def contentSize: Int = ByteBytes + IntBytes + value.length
}
private final case class BooleanData(value: Boolean) extends DataEntry {
  override def write(builder: ByteStringBuilder): Unit = WriteOps.writeBoolean(value)(builder)
  override def contentSize: Int = ByteBytes + ByteBytes
}
private final case class ListData private(value: IQueue[DataEntry], contentSizeWithoutHeader: Int) extends DataEntry {
  override def write(builder: ByteStringBuilder): Unit = {
    builder += ListStartMarker.byte
    builder.putInt(contentSizeWithoutHeader)
    value.foreach(_.write(builder))
  }
  override def contentSize: Int = ByteBytes + IntBytes + contentSizeWithoutHeader

  def append(entry: DataEntry): ListData = copy(value.enqueue(entry), contentSizeWithoutHeader + entry.contentSize)

}
private object ListData {
  def empty = ListData(IQueue.empty, 0)
}

private final case class ObjectData(value: IQueue[(StringData, DataEntry)], contentSizeWithoutHeader: Int) extends DataEntry {
  override def write(builder: ByteStringBuilder): Unit = {
    builder += ObjectStartMarker.byte
    builder.putInt(contentSizeWithoutHeader)
    value.foreach {
      case (key, entry) =>
        key.write(builder)
        entry.write(builder)
    }
  }
  override def contentSize: Int = ByteBytes + IntBytes + contentSizeWithoutHeader

  def append(entry: (StringData, DataEntry)) = copy(value.enqueue(entry), contentSizeWithoutHeader + entry._1.contentSize + entry._2.contentSize)
}

private object ObjectData {
  def empty = ObjectData(IQueue.empty, 0)
}

private sealed trait DataEntryWriter[T <: DataEntry] {
  def write(builder: ByteStringBuilder, elem: T): Unit
}

private final class DirectByteArrayListOutput(onFinish: ByteString => Unit) extends ListOutput {

  private var data = ListData.empty

  override def writeElement(): Output = new LazyEvaluatedOutput(entry => {
    data = data.append(entry)
  })

  override def finish(): Unit = {
    val builder = ByteString.newBuilder
    data.write(builder)
    onFinish(builder.result())
  }

}

private final class DirectByteStringObjectOutput(onFinish: ByteString => Unit) extends ObjectOutput {

  private var data = ObjectData.empty

  override def writeField(key: String): Output = new LazyEvaluatedOutput(entry => {
    data = data.append(StringData(key) -> entry)
  })
  override def finish(): Unit = {
    val builder = ByteString.newBuilder
    data.write(builder)
    onFinish(builder.result())
  }
}

private final class LazyEvaluatedOutput(onFinish: DataEntry => Unit) extends Output {
  override def writeNull(): Unit = onFinish(NullData)
  override def writeString(str: String): Unit = onFinish(StringData(str))
  override def writeByte(byte: Byte): Unit = onFinish(ByteData(byte))
  override def writeShort(short: Short): Unit = onFinish(ShortData(short))
  override def writeInt(int: Int): Unit = onFinish(IntData(int))
  override def writeLong(long: Long): Unit = onFinish(LongData(long))
  override def writeFloat(float: Float): Unit = onFinish(FloatData(float))
  override def writeDouble(double: Double): Unit = onFinish(DoubleData(double))
  override def writeBinary(binary: Array[Byte]): Unit = onFinish(ByteArrayData(binary))
  override def writeBoolean(boolean: Boolean): Unit = onFinish(BooleanData(boolean))
  override def writeList(): ListOutput = new LazyEvaluatedListOutput(entry => onFinish(entry))
  override def writeObject(): ObjectOutput = new LazyEvaluatedObjectOutput(entry => onFinish(entry))
}

private final class LazyEvaluatedListOutput(onFinish: DataEntry => Unit) extends ListOutput {

  private var data = ListData.empty
  override def writeElement(): Output = new LazyEvaluatedOutput(entry => {
    data = data.append(entry)
  })
  override def finish(): Unit = onFinish(data)
}

private final class LazyEvaluatedObjectOutput(onFinish: DataEntry => Unit) extends ObjectOutput {
  private var data = ObjectData.empty

  override def writeField(key: String): Output = new LazyEvaluatedOutput(entry => {
    data = data.append(StringData(key) -> entry)
  })
  override def finish(): Unit = onFinish(data)
}
