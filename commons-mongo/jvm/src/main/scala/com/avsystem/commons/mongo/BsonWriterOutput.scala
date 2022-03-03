package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization.{ListOutput, ObjectOutput}
import org.bson.types.{Decimal128, ObjectId}
import org.bson.{BsonBinary, BsonWriter}

final class BsonWriterOutput(bw: BsonWriter, override val legacyOptionEncoding: Boolean = false)
  extends BsonOutput {

  override def writeNull(): Unit =
    bw.writeNull()

  override def writeString(str: String): Unit =
    bw.writeString(str)

  override def writeBoolean(boolean: Boolean): Unit =
    bw.writeBoolean(boolean)

  override def writeInt(int: Int): Unit =
    bw.writeInt32(int)

  override def writeLong(long: Long): Unit =
    if (long.isValidInt) writeInt(long.toInt)
    else bw.writeInt64(long)

  override def writeTimestamp(millis: Long): Unit =
    bw.writeDateTime(millis)

  override def writeDouble(double: Double): Unit =
    bw.writeDouble(double)

  override def writeBinary(binary: Array[Byte]): Unit =
    bw.writeBinaryData(new BsonBinary(binary))

  override def writeList(): BsonWriterListOutput = {
    bw.writeStartArray()
    new BsonWriterListOutput(bw, legacyOptionEncoding)
  }

  override def writeObject(): BsonWriterObjectOutput = {
    bw.writeStartDocument()
    new BsonWriterObjectOutput(bw, legacyOptionEncoding)
  }

  override def writeObjectId(objectId: ObjectId): Unit =
    bw.writeObjectId(objectId)

  override def writeDecimal128(decimal128: Decimal128): Unit =
    bw.writeDecimal128(decimal128)
}

final class BsonWriterNamedOutput(escapedName: String, bw: BsonWriter, override val legacyOptionEncoding: Boolean)
  extends BsonOutput {

  override def writeNull(): Unit =
    bw.writeNull(escapedName)

  override def writeString(str: String): Unit =
    bw.writeString(escapedName, str)

  override def writeBoolean(boolean: Boolean): Unit =
    bw.writeBoolean(escapedName, boolean)

  override def writeInt(int: Int): Unit =
    bw.writeInt32(escapedName, int)

  override def writeLong(long: Long): Unit =
    if (long.isValidInt) writeInt(long.toInt)
    else bw.writeInt64(escapedName, long)

  override def writeTimestamp(millis: Long): Unit =
    bw.writeDateTime(escapedName, millis)

  override def writeDouble(double: Double): Unit =
    bw.writeDouble(escapedName, double)

  override def writeBinary(binary: Array[Byte]): Unit =
    bw.writeBinaryData(escapedName, new BsonBinary(binary))

  override def writeList(): BsonWriterListOutput = {
    bw.writeStartArray(escapedName)
    new BsonWriterListOutput(bw, legacyOptionEncoding)
  }

  override def writeObject(): BsonWriterObjectOutput = {
    bw.writeName(escapedName) // org.bson.BsonWriter.writeStartDocument(java.lang.String) fails when writing _id in 4.0 driver
    bw.writeStartDocument()
    new BsonWriterObjectOutput(bw, legacyOptionEncoding)
  }

  override def writeObjectId(objectId: ObjectId): Unit =
    bw.writeObjectId(escapedName, objectId)

  override def writeDecimal128(decimal128: Decimal128): Unit =
    bw.writeDecimal128(escapedName, decimal128)
}

final class BsonWriterListOutput(bw: BsonWriter, legacyOptionEncoding: Boolean) extends ListOutput {
  override def writeElement() =
    new BsonWriterOutput(bw, legacyOptionEncoding)

  override def finish(): Unit =
    bw.writeEndArray()
}

final class BsonWriterObjectOutput(bw: BsonWriter, legacyOptionEncoding: Boolean) extends ObjectOutput {
  override def writeField(key: String) =
    new BsonWriterNamedOutput(KeyEscaper.escape(key), bw, legacyOptionEncoding)

  override def finish(): Unit =
    bw.writeEndDocument()
}
