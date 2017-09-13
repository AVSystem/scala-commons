package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization.{ListOutput, ObjectOutput}
import org.bson.types.ObjectId
import org.bson.{BsonBinary, BsonWriter}

class BsonWriterOutput(bw: BsonWriter) extends BsonOutput {
  override def writeNull(): Unit = bw.writeNull()
  override def writeString(str: String): Unit = bw.writeString(str)
  override def writeBoolean(boolean: Boolean): Unit = bw.writeBoolean(boolean)
  override def writeInt(int: Int): Unit = bw.writeInt32(int)
  override def writeLong(long: Long): Unit = bw.writeInt64(long)
  override def writeTimestamp(millis: Long): Unit = bw.writeDateTime(millis)
  override def writeDouble(double: Double): Unit = bw.writeDouble(double)
  override def writeBinary(binary: Array[Byte]): Unit = bw.writeBinaryData(new BsonBinary(binary))
  override def writeList(): BsonWriterListOutput = BsonWriterListOutput.startWriting(bw)
  override def writeObject(): BsonWriterObjectOutput = BsonWriterObjectOutput.startWriting(bw)
  override def writeObjectId(objectId: ObjectId): Unit = bw.writeObjectId(objectId)
}

class BsonWriterNamedOutput(name: String, bw: BsonWriter) extends BsonOutput {
  override def writeNull(): Unit = bw.writeNull(name)
  override def writeString(str: String): Unit = bw.writeString(name, str)
  override def writeBoolean(boolean: Boolean): Unit = bw.writeBoolean(name, boolean)
  override def writeInt(int: Int): Unit = bw.writeInt32(name, int)
  override def writeLong(long: Long): Unit = bw.writeInt64(name, long)
  override def writeTimestamp(millis: Long): Unit = bw.writeDateTime(name, millis)
  override def writeDouble(double: Double): Unit = bw.writeDouble(name, double)
  override def writeBinary(binary: Array[Byte]): Unit = bw.writeBinaryData(name, new BsonBinary(binary))
  override def writeList(): BsonWriterListOutput = BsonWriterListOutput.startWriting(bw, name)
  override def writeObject(): BsonWriterObjectOutput = BsonWriterObjectOutput.startWriting(bw, name)
  override def writeObjectId(objectId: ObjectId): Unit = bw.writeObjectId(name, objectId)
}

class BsonWriterListOutput private(bw: BsonWriter) extends ListOutput {
  override def writeElement() = new BsonWriterOutput(bw)
  override def finish(): Unit = bw.writeEndArray()
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
  override def finish(): Unit = bw.writeEndDocument()
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
