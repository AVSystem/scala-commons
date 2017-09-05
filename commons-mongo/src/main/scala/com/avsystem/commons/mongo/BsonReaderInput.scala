package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization._
import com.google.common.collect.AbstractIterator
import org.bson.{BsonReader, BsonType}

class BsonReaderInput(br: BsonReader) extends Input {
  override def inputType = br.getCurrentBsonType match {
    case BsonType.NULL => InputType.Null
    case BsonType.ARRAY => InputType.List
    case BsonType.DOCUMENT => InputType.Object
    case _ => InputType.Simple
  }

  override def readNull() = {
    br.readNull()
    null
  }
  override def readString() = br.readString()
  override def readBoolean() = br.readBoolean()
  override def readInt() = br.readInt32()
  override def readLong() = br.readInt64()
  override def readTimestamp() = br.readDateTime()
  override def readDouble() = br.readDouble()
  override def readBinary() = br.readBinaryData().getData
  override def readList() = BsonReaderListInput.startReading(br)
  override def readObject() = BsonReaderObjectInput.startReading(br)
  override def skip() = br.skipValue()
}

class BsonReaderFieldInput(name: String, br: BsonReader) extends BsonReaderInput(br) with FieldInput {
  override def fieldName = name
}

class BsonReaderIterator[T](br: BsonReader, endCallback: BsonReader => Unit, readElement: BsonReader => T)
  extends AbstractIterator[T] {
  override def computeNext() = {
    if (br.readBsonType() == BsonType.END_OF_DOCUMENT) {
      endCallback(br)
      endOfData()
    } else {
      readElement(br)
    }
  }
}

class BsonReaderListInput private(br: BsonReader) extends ListInput {
  private val it = new BsonReaderIterator(br, _.readEndArray(), new BsonReaderInput(_))

  override def hasNext = it.hasNext
  override def nextElement() = it.next()
}
object BsonReaderListInput {
  def startReading(br: BsonReader): BsonReaderListInput = {
    br.readStartArray()
    new BsonReaderListInput(br)
  }
}

class BsonReaderObjectInput private(br: BsonReader) extends ObjectInput {
  private val it = new BsonReaderIterator(br, _.readEndDocument(),
    br => new BsonReaderFieldInput(br.readName(), br)
  )

  override def hasNext = it.hasNext
  override def nextField() = it.next()
}
object BsonReaderObjectInput {
  def startReading(br: BsonReader): BsonReaderObjectInput = {
    br.readStartDocument()
    new BsonReaderObjectInput(br)
  }
}
