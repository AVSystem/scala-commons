package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization.{ListOutput, ObjectOutput}
import org.bson.types.ObjectId
import org.bson.{BsonBinary, BsonWriter}

final class BsonWriterOutput(bw: BsonWriter) extends BsonOutput {
  override def writeNull(): Unit = bw.writeNull()
  override def writeString(str: String): Unit = bw.writeString(str)
  override def writeBoolean(boolean: Boolean): Unit = bw.writeBoolean(boolean)
  override def writeInt(int: Int): Unit = bw.writeInt32(int)
  override def writeLong(long: Long): Unit = bw.writeInt64(long)
  override def writeTimestamp(millis: Long): Unit = bw.writeDateTime(millis)
  override def writeDouble(double: Double): Unit = bw.writeDouble(double)
  override def writeBinary(binary: Array[Byte]): Unit = bw.writeBinaryData(new BsonBinary(binary))
  override def writeList(): BsonWriterListOutput = {
    bw.writeStartArray()
    new BsonWriterListOutput(bw)
  }
  override def writeObject(): BsonWriterObjectOutput = {
    bw.writeStartDocument()
    new BsonWriterObjectOutput(bw)
  }
  override def writeObjectId(objectId: ObjectId): Unit = bw.writeObjectId(objectId)
}

final class BsonWriterNamedOutput(escapedName: String, bw: BsonWriter) extends BsonOutput {
  override def writeNull(): Unit = bw.writeNull(escapedName)
  override def writeString(str: String): Unit = bw.writeString(escapedName, str)
  override def writeBoolean(boolean: Boolean): Unit = bw.writeBoolean(escapedName, boolean)
  override def writeInt(int: Int): Unit = bw.writeInt32(escapedName, int)
  override def writeLong(long: Long): Unit = bw.writeInt64(escapedName, long)
  override def writeTimestamp(millis: Long): Unit = bw.writeDateTime(escapedName, millis)
  override def writeDouble(double: Double): Unit = bw.writeDouble(escapedName, double)
  override def writeBinary(binary: Array[Byte]): Unit = bw.writeBinaryData(escapedName, new BsonBinary(binary))
  override def writeList(): BsonWriterListOutput = {
    bw.writeStartArray(escapedName)
    new BsonWriterListOutput(bw)
  }
  override def writeObject(): BsonWriterObjectOutput = {
    bw.writeStartDocument(escapedName)
    new BsonWriterObjectOutput(bw)
  }
  override def writeObjectId(objectId: ObjectId): Unit = bw.writeObjectId(escapedName, objectId)
}

final class BsonWriterListOutput(bw: BsonWriter) extends ListOutput {
  override def writeElement() = new BsonWriterOutput(bw)
  override def finish(): Unit = bw.writeEndArray()
}

final class BsonWriterObjectOutput(bw: BsonWriter) extends ObjectOutput {
  override def writeField(key: String) = new BsonWriterNamedOutput(KeyEscaper.escape(key), bw)
  override def finish(): Unit = bw.writeEndDocument()
}
