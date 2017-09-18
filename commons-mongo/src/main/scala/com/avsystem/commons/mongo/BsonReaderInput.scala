package com.avsystem.commons
package mongo

import java.util.NoSuchElementException

import com.avsystem.commons.serialization.{FieldInput, InputType, ListInput, ObjectInput}
import com.google.common.collect.AbstractIterator
import org.bson.types.ObjectId
import org.bson.{BsonReader, BsonType}

class BsonReaderInput(br: BsonReader) extends BsonInput {
  override def inputType: InputType = br.getCurrentBsonType match {
    case BsonType.NULL => InputType.Null
    case BsonType.ARRAY => InputType.List
    case BsonType.DOCUMENT => InputType.Object
    case _ => InputType.Simple
  }

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
  override def readBinary(): Array[Byte] = br.readBinaryData().getData
  override def readList(): BsonReaderListInput = BsonReaderListInput.startReading(br)
  override def readObject(): BsonReaderObjectInput = BsonReaderObjectInput.startReading(br)
  override def readObjectId(): ObjectId = br.readObjectId()
  override def skip(): Unit = br.skipValue()
}

class BsonReaderFieldInput(name: String, br: BsonReader) extends BsonReaderInput(br) with FieldInput {
  override def fieldName: String = name
}

class ObjectIdFieldInput(name: String, objectId: ObjectId) extends BsonInput with FieldInput {
  override def fieldName: String = name
  override def readObjectId(): ObjectId = objectId
  override def inputType: InputType = InputType.Simple
  override def skip(): Unit = ()

  override def readNull(): Null = throw new UnsupportedOperationException
  override def readString(): String = throw new UnsupportedOperationException
  override def readBoolean(): Boolean = throw new UnsupportedOperationException
  override def readInt(): Int = throw new UnsupportedOperationException
  override def readLong(): Long = throw new UnsupportedOperationException
  override def readDouble(): Double = throw new UnsupportedOperationException
  override def readBinary(): Array[Byte] = throw new UnsupportedOperationException
  override def readList(): ListInput = throw new UnsupportedOperationException
  override def readObject(): ObjectInput = throw new UnsupportedOperationException
}

class BsonReaderIterator[T](br: BsonReader, endCallback: BsonReader => Unit, readElement: BsonReader => T)
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

class BsonReaderListInput private(br: BsonReader) extends ListInput {
  private val it = new BsonReaderIterator(br, _.readEndArray(), new BsonReaderInput(_))

  override def hasNext: Boolean = it.hasNext
  override def nextElement(): BsonReaderInput = it.next()
}
object BsonReaderListInput {
  def startReading(br: BsonReader): BsonReaderListInput = {
    br.readStartArray()
    new BsonReaderListInput(br)
  }
}


class BsonReaderObjectInput private(br: BsonReader) extends ObjectInput {
  private val it = new BsonReaderIterator(br, _.readEndDocument(),
    br => new BsonReaderFieldInput(KeyEscaper.unescape(br.readName()), br)
  )

  val objectId: Opt[ObjectId] =
    if (it.hasNext && it.peek().fieldName == "_id") Opt.some(it.next().readObjectId())
    else Opt.empty

  private var idToRead = objectId.isDefined

  override def hasNext: Boolean = it.hasNext || idToRead
  override def nextField(): FieldInput =
    if (it.hasNext) {
      it.next()
    }
    else if (idToRead) {
      idToRead = false
      new ObjectIdFieldInput("_id", objectId.get)
    } else {
      throw new NoSuchElementException
    }
}

object BsonReaderObjectInput {
  def startReading(br: BsonReader): BsonReaderObjectInput = {
    br.readStartDocument()
    new BsonReaderObjectInput(br)
  }
}
