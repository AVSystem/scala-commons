package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization.{ListOutput, ObjectOutput, Output}
import org.bson.{BsonBinary, BsonWriter}

class BsonWriterOutput(bw: BsonWriter) extends Output {
  override def writeNull() = bw.writeNull()
  override def writeString(str: String) = bw.writeString(str)
  override def writeBoolean(boolean: Boolean) = bw.writeBoolean(boolean)
  override def writeInt(int: Int) = bw.writeInt32(int)
  override def writeLong(long: Long) = bw.writeInt64(long)
  override def writeTimestamp(millis: Long) = bw.writeDateTime(millis)
  override def writeDouble(double: Double) = bw.writeDouble(double)
  override def writeBinary(binary: Array[Byte]) = bw.writeBinaryData(new BsonBinary(binary))
  override def writeList() = BsonWriterListOutput.startWriting(bw)
  override def writeObject() = BsonWriterObjectOutput.startWriting(bw)
}

class BsonWriterNamedOutput(name: String, bw: BsonWriter) extends Output {
  override def writeNull() = bw.writeNull(name)
  override def writeString(str: String) = bw.writeString(name, str)
  override def writeBoolean(boolean: Boolean) = bw.writeBoolean(name, boolean)
  override def writeInt(int: Int) = bw.writeInt32(name, int)
  override def writeLong(long: Long) = bw.writeInt64(name, long)
  override def writeTimestamp(millis: Long) = bw.writeDateTime(name, millis)
  override def writeDouble(double: Double) = bw.writeDouble(name, double)
  override def writeBinary(binary: Array[Byte]) = bw.writeBinaryData(name, new BsonBinary(binary))
  override def writeList() = BsonWriterListOutput.startWriting(bw, name)
  override def writeObject() = BsonWriterObjectOutput.startWriting(bw, name)
}

class BsonWriterListOutput private(bw: BsonWriter) extends ListOutput {
  override def writeElement() = new BsonWriterOutput(bw)
  override def finish() = bw.writeEndArray()
}
object BsonWriterListOutput {
  def startWriting(bw: BsonWriter): BsonWriterListOutput = {
    bw.writeStartArray()
    new BsonWriterListOutput(bw)
  }

  def startWriting(bw: BsonWriter, name: String): BsonWriterListOutput = {
    bw.writeStartArray(name)
    new BsonWriterListOutput(bw)
  }
}

class BsonWriterObjectOutput private(bw: BsonWriter) extends ObjectOutput {
  override def writeField(key: String) = new BsonWriterNamedOutput(key, bw)
  override def finish() = bw.writeEndDocument()
}
object BsonWriterObjectOutput {
  def startWriting(bw: BsonWriter): BsonWriterObjectOutput = {
    bw.writeStartDocument()
    new BsonWriterObjectOutput(bw)
  }

  def startWriting(bw: BsonWriter, name: String): BsonWriterObjectOutput = {
    bw.writeStartDocument(name)
    new BsonWriterObjectOutput(bw)
  }
}
