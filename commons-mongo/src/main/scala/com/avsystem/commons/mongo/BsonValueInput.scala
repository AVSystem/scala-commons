package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization.{FieldInput, GenCodec, Input, ListInput, ObjectInput}
import org.bson._
import org.bson.types.ObjectId

object BsonValueInput {
  def read[T: GenCodec](bsonValue: BsonValue, legacyOptionEncoding: Boolean = false): T =
    GenCodec.read[T](new BsonValueInput(bsonValue, legacyOptionEncoding))
}

class BsonValueInput(bsonValue: BsonValue, override val legacyOptionEncoding: Boolean = false) extends BsonInput {
  protected def bsonType: BsonType = bsonValue.getBsonType

  private def handleFailures[T](expr: => T): T =
    try expr catch {
      case e: BsonInvalidOperationException => throw new ReadFailure(e.getMessage, e)
    }

  def readString(): String = handleFailures(bsonValue.asString().getValue)
  def readBoolean(): Boolean = handleFailures(bsonValue.asBoolean().getValue)
  def readInt(): Int = handleFailures(bsonValue.asInt32().getValue)
  def readLong(): Long = handleFailures(bsonValue.asInt64().getValue)
  override def readTimestamp(): Long = handleFailures(bsonValue.asDateTime().getValue)
  def readDouble(): Double = handleFailures(bsonValue.asDouble().getValue)
  def readBigInt(): BigInt = handleFailures(BigInt(bsonValue.asBinary().getData))
  def readBigDecimal(): BigDecimal = handleFailures(BsonInput.bigDecimalFromBytes(bsonValue.asBinary().getData))
  def readBinary(): Array[Byte] = handleFailures(bsonValue.asBinary().getData)
  def readObjectId(): ObjectId = handleFailures(bsonValue.asObjectId().getValue)

  def readNull(): Boolean = bsonValue == BsonNull.VALUE
  def readList(): ListInput = new BsonValueListInput(handleFailures(bsonValue.asArray()), legacyOptionEncoding)
  def readObject(): ObjectInput = new BsonValueObjectInput(handleFailures(bsonValue.asDocument()), legacyOptionEncoding)
  def skip(): Unit = ()
}

class BsonValueFieldInput(val fieldName: String, bsonValue: BsonValue, legacyOptionEncoding: Boolean)
  extends BsonValueInput(bsonValue, legacyOptionEncoding) with FieldInput

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
