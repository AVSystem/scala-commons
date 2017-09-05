package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization.{ListOutput, ObjectOutput, Output}
import org.bson.{BsonArray, BsonBinary, BsonBoolean, BsonDocument, BsonDouble, BsonInt32, BsonInt64, BsonNull, BsonString, BsonValue}

class BsonValueOutput extends Output {
  private var _value: Opt[BsonValue] = Opt.empty

  private def setValue(bsonValue: BsonValue): Unit = {
    _value match {
      case Opt.Empty =>
        _value = Opt.some(bsonValue)
      case Opt(oldValue) =>
        throw new IllegalStateException(s"Cannot set value to $bsonValue, value is already present: $oldValue")
    }
  }

  def value: BsonValue = {
    _value match {
      case Opt.Empty =>
        throw new IllegalStateException("Value is not set")
      case Opt(bsonValue) =>
        bsonValue
    }
  }

  override def writeNull(): Unit = setValue(BsonNull.VALUE)
  override def writeString(str: String): Unit = setValue(new BsonString(str))
  override def writeBoolean(boolean: Boolean): Unit = setValue(BsonBoolean.valueOf(boolean))
  override def writeInt(int: Int): Unit = setValue(new BsonInt32(int))
  override def writeLong(long: Long): Unit = setValue(new BsonInt64(long))
  override def writeDouble(double: Double): Unit = setValue(new BsonDouble(double))
  override def writeBinary(binary: Array[Byte]): Unit = setValue(new BsonBinary(binary))
  override def writeList(): ListOutput = new BsonValueListOutput(setValue)
  override def writeObject(): ObjectOutput = new BsonValueObjectOutput(setValue)
}

class BsonValueListOutput(receiver: BsonArray => Unit) extends ListOutput {
  private var lastOutput: Opt[BsonValueOutput] = Opt.empty
  private val array = new BsonArray()

  private def flushLast(): Unit = {
    lastOutput.foreach(out => array.add(out.value))
  }

  override def writeElement(): Output = {
    flushLast()
    val out = new BsonValueOutput
    lastOutput = Opt.some(out)
    out
  }

  override def finish(): Unit = {
    flushLast()
    lastOutput = Opt.empty
    receiver(array)
  }
}

class BsonValueObjectOutput(receiver: BsonDocument => Unit) extends ObjectOutput {
  private var lastOutput: Opt[(String, BsonValueOutput)] = Opt.empty
  private val doc = new BsonDocument()

  private def flushLast(): Unit = {
    lastOutput.foreach {
      case (name, out) => doc.put(name, out.value)
    }
  }

  override def writeField(key: String): Output = {
    flushLast()
    val out = new BsonValueOutput
    lastOutput = Opt.some(key -> out)
    out
  }

  override def finish(): Unit = {
    flushLast()
    lastOutput = Opt.empty
    receiver(doc)
  }
}
