package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization.{FieldInput, LegacyOptionEncodingInput, ListInput, ObjectInput}
import com.google.common.collect.AbstractIterator
import org.bson.types.ObjectId
import org.bson.{BsonReader, BsonType}

class BsonReaderInput(br: BsonReader, legacyOptionEncoding: Boolean = false)
  extends BsonInput with LegacyOptionEncodingInput {

  override def legacyEncodingEnabled: Boolean = legacyOptionEncoding

  override def isNull: Boolean =
    br.getCurrentBsonType == BsonType.NULL
  override def readNull(): Null = {
    br.readNull()
    null
  }
  override def readString(): String = br.readString()
  override def readBoolean(): Boolean = br.readBoolean()
  override def readInt(): Int = br.readInt32()
  override def readLong(): Long = br.readInt64()
  override def readTimestamp(): Long = br.readDateTime()
  override def readDouble(): Double = br.readDouble()
  override def readBigInt(): BigInt = BigInt(br.readBinaryData().getData)
  override def readBigDecimal(): BigDecimal = BsonInput.bigDecimalFromBytes(br.readBinaryData().getData)
  override def readBinary(): Array[Byte] = br.readBinaryData().getData
  override def readList(): BsonReaderListInput = {
    br.readStartArray()
    new BsonReaderListInput(new BsonReaderIterator(br, _.readEndArray(), new BsonReaderInput(_, legacyOptionEncoding)))
  }
  override def readObject(): BsonReaderObjectInput = {
    br.readStartDocument()
    new BsonReaderObjectInput(new BsonReaderIterator(br, _.readEndDocument(),
      br => new BsonReaderFieldInput(KeyEscaper.unescape(br.readName()), br, legacyOptionEncoding)
    ))
  }
  override def readObjectId(): ObjectId = br.readObjectId()
  override def skip(): Unit = br.skipValue()
}

final class BsonReaderFieldInput(name: String, br: BsonReader, legacyOptionEncoding: Boolean)
  extends BsonReaderInput(br, legacyOptionEncoding) with FieldInput {
  override def fieldName: String = name
}

final class BsonReaderIterator[T](br: BsonReader, endCallback: BsonReader => Unit, readElement: BsonReader => T)
  extends AbstractIterator[T] {
  override def computeNext(): T = {
    if (br.readBsonType() == BsonType.END_OF_DOCUMENT) {
      endCallback(br)
      endOfData()
    } else {
      readElement(br)
    }
  }
}

final class BsonReaderListInput(it: BsonReaderIterator[BsonReaderInput]) extends ListInput {
  override def hasNext: Boolean = it.hasNext
  override def nextElement(): BsonReaderInput = it.next()
}

final class BsonReaderObjectInput(it: BsonReaderIterator[BsonReaderFieldInput]) extends ObjectInput {
  override def hasNext: Boolean = it.hasNext
  override def nextField(): BsonReaderFieldInput = it.next()
}
