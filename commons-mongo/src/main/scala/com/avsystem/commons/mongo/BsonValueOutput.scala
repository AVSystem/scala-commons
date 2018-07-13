package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization.{GenCodec, ListOutput, ObjectOutput}
import org.bson._
import org.bson.types.ObjectId

object BsonValueOutput {
  def write[T: GenCodec](value: T, legacyOptionEncoding: Boolean = false): BsonValue = {
    var result: BsonValue = null
    GenCodec.write(new BsonValueOutput(result = _, legacyOptionEncoding), value)
    result
  }
}

final class BsonValueOutput(receiver: BsonValue => Unit = _ => (), override val legacyOptionEncoding: Boolean = false)
  extends BsonOutput {

  private var _value: Opt[BsonValue] = Opt.empty

  private def setValue(bsonValue: BsonValue): Unit = {
    _value match {
      case Opt.Empty =>
        _value = Opt.some(bsonValue)
        receiver(bsonValue)
      case Opt(oldValue) =>
        throw new IllegalStateException(s"Cannot set value to $bsonValue, value is already present: $oldValue")
    }
  }

  override def writeNull(): Unit = setValue(BsonNull.VALUE)
  override def writeString(str: String): Unit = setValue(new BsonString(str))
  override def writeBoolean(boolean: Boolean): Unit = setValue(BsonBoolean.valueOf(boolean))
  override def writeInt(int: Int): Unit = setValue(new BsonInt32(int))
  override def writeLong(long: Long): Unit = setValue(new BsonInt64(long))
  override def writeTimestamp(millis: Long): Unit = setValue(new BsonDateTime(millis))
  override def writeDouble(double: Double): Unit = setValue(new BsonDouble(double))
  override def writeBigInt(bigInt: BigInt): Unit = setValue(new BsonBinary(bigInt.toByteArray))
  override def writeBigDecimal(bigDecimal: BigDecimal): Unit = setValue(new BsonBinary(BsonOutput.bigDecimalBytes(bigDecimal)))
  override def writeBinary(binary: Array[Byte]): Unit = setValue(new BsonBinary(binary))
  override def writeList(): ListOutput = new BsonValueListOutput(setValue, legacyOptionEncoding)
  override def writeObject(): ObjectOutput = new BsonValueObjectOutput(setValue, legacyOptionEncoding)
  override def writeObjectId(objectId: ObjectId): Unit = setValue(new BsonObjectId(objectId))
}

final class BsonValueListOutput(receiver: BsonArray => Unit, legacyOptionEncoding: Boolean)
  extends ListOutput {
  private val array = new BsonArray()

  override def writeElement(): BsonOutput =
    new BsonValueOutput(v => array.add(v), legacyOptionEncoding)

  override def finish(): Unit = receiver(array)
}

final class BsonValueObjectOutput(receiver: BsonDocument => Unit, legacyOptionEncoding: Boolean)
  extends ObjectOutput {
  private val doc = new BsonDocument()

  override def writeField(key: String): BsonOutput =
    new BsonValueOutput(v => doc.put(KeyEscaper.escape(key), v), legacyOptionEncoding)

  override def finish(): Unit = receiver(doc)
}
