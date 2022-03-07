package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization._
import org.bson._
import org.bson.types.{Decimal128, ObjectId}

object BsonValueInput {
  def read[T: GenCodec](bsonValue: BsonValue, legacyOptionEncoding: Boolean = false): T =
    GenCodec.read[T](new BsonValueInput(bsonValue, legacyOptionEncoding))
}

class BsonValueInput(bsonValue: BsonValue, override val legacyOptionEncoding: Boolean = false) extends BsonInput {
  protected def bsonType: BsonType = bsonValue.getBsonType

  def readString(): String =
    expect(BsonType.STRING, bsonValue.asString().getValue)

  def readBoolean(): Boolean =
    expect(BsonType.BOOLEAN, bsonValue.asBoolean().getValue)

  def readInt(): Int =
    expect(BsonType.INT32, bsonValue.asInt32().getValue)

  def readLong(): Long = handleFailures {
    bsonType match {
      case BsonType.INT32 => readInt().toLong
      case BsonType.INT64 => bsonValue.asInt64().getValue
      case _ => wrongType(BsonType.INT32, BsonType.INT64)
    }
  }

  override def readTimestamp(): Long =
    expect(BsonType.DATE_TIME, bsonValue.asDateTime().getValue)

  def readDouble(): Double =
    expect(BsonType.DOUBLE, bsonValue.asDouble().getValue)

  def readBinary(): Array[Byte] =
    expect(BsonType.BINARY, bsonValue.asBinary().getData)

  def readObjectId(): ObjectId =
    expect(BsonType.OBJECT_ID, bsonValue.asObjectId().getValue)

  def readDecimal128(): Decimal128 =
    expect(BsonType.DECIMAL128, bsonValue.asDecimal128().getValue)

  def readBsonValue(): BsonValue =
    bsonValue

  def readNull(): Boolean =
    bsonValue == BsonNull.VALUE

  def readList(): ListInput =
    new BsonValueListInput(expect(BsonType.ARRAY, bsonValue.asArray()), legacyOptionEncoding)

  def readObject(): ObjectInput =
    new BsonValueObjectInput(expect(BsonType.DOCUMENT, bsonValue.asDocument()), legacyOptionEncoding)

  def skip(): Unit = ()
}

class BsonValueFieldInput(val fieldName: String, bsonValue: BsonValue, legacyOptionEncoding: Boolean)
  extends BsonValueInput(bsonValue, legacyOptionEncoding) with BsonFieldInput

class BsonValueListInput(bsonArray: BsonArray, legacyOptionEncoding: Boolean) extends ListInput {
  private val it = bsonArray.iterator

  def hasNext: Boolean = it.hasNext
  def nextElement(): Input = new BsonValueInput(it.next(), legacyOptionEncoding)
}

class BsonValueObjectInput(bsonDocument: BsonDocument, legacyOptionEncoding: Boolean) extends ObjectInput {
  private val it = bsonDocument.entrySet.iterator

  def hasNext: Boolean = it.hasNext

  def nextField(): FieldInput = {
    val e = it.next()
    new BsonValueFieldInput(KeyEscaper.unescape(e.getKey), e.getValue, legacyOptionEncoding)
  }

  override def peekField(name: String): Opt[FieldInput] =
    bsonDocument.get(KeyEscaper.escape(name)).opt.map(new BsonValueFieldInput(name, _, legacyOptionEncoding))
}
