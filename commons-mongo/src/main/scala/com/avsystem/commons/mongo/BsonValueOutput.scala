package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization.{ListOutput, ObjectOutput, Output}
import org.bson._

class BsonValueOutput(receiver: BsonValue => Unit = _ => ()) extends Output {
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

  def value: BsonValue = _value.getOrElse(throw new IllegalStateException("Value is not set"))

  override def writeNull(): Unit = setValue(BsonNull.VALUE)
  override def writeString(str: String): Unit = setValue(new BsonString(str))
  override def writeBoolean(boolean: Boolean): Unit = setValue(BsonBoolean.valueOf(boolean))
  override def writeInt(int: Int): Unit = setValue(new BsonInt32(int))
  override def writeLong(long: Long): Unit = setValue(new BsonInt64(long))
  override def writeTimestamp(millis: Long): Unit = setValue(new BsonDateTime(millis))
  override def writeDouble(double: Double): Unit = setValue(new BsonDouble(double))
  override def writeBinary(binary: Array[Byte]): Unit = setValue(new BsonBinary(binary))
  override def writeList(): ListOutput = new BsonValueListOutput(setValue)
  override def writeObject(): ObjectOutput = new BsonValueObjectOutput(setValue)
}

class BsonValueListOutput(receiver: BsonArray => Unit) extends ListOutput {
  private val array = new BsonArray()

  override def writeElement(): Output = new BsonValueOutput(v => array.add(v))
  override def finish(): Unit = receiver(array)
}

class BsonValueObjectOutput(receiver: BsonDocument => Unit) extends ObjectOutput {
  private val doc = new BsonDocument()

  override def writeField(key: String): Output = new BsonValueOutput(v => doc.put(key, v))
  override def finish(): Unit = receiver(doc)
}
