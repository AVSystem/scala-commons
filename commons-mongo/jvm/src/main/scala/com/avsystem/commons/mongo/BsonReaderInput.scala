package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization.{FieldInput, ListInput, ObjectInput}
import com.google.common.collect.AbstractIterator
import org.bson.types.{Decimal128, ObjectId}
import org.bson.{BsonReader, BsonType, BsonValue}

class BsonReaderInput(br: BsonReader, override val legacyOptionEncoding: Boolean = false)
  extends BsonInput {

  override def readNull(): Boolean =
    bsonType == BsonType.NULL && {
      br.readNull()
      true
    }

  override def readString(): String =
    expect(BsonType.STRING, br.readString())

  override def readBoolean(): Boolean =
    expect(BsonType.BOOLEAN, br.readBoolean())

  override def readInt(): Int =
    expect(BsonType.INT32, br.readInt32())

  override def readLong(): Long = bsonType match {
    case BsonType.INT32 => br.readInt32().toLong
    case BsonType.INT64 => br.readInt64()
    case _ => wrongType(BsonType.INT32, BsonType.INT64)
  }

  override def readTimestamp(): Long =
    expect(BsonType.DATE_TIME, br.readDateTime())

  override def readDouble(): Double =
    expect(BsonType.DOUBLE, br.readDouble())

  override def readBinary(): Array[Byte] =
    expect(BsonType.BINARY, br.readBinaryData().getData)

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

  override def readObjectId(): ObjectId =
    expect(BsonType.OBJECT_ID, br.readObjectId())

  override def readDecimal128(): Decimal128 =
    expect(BsonType.DECIMAL128, br.readDecimal128())

  override def readBsonValue(): BsonValue =
    BsonValueUtils.decode(br)

  override def skip(): Unit =
    br.skipValue()

  override protected final def bsonType: BsonType = br.getCurrentBsonType match {
    case null => br.readBsonType() // reader may be in a state where the type hasn't been read yet
    case bsonType => bsonType
  }
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
