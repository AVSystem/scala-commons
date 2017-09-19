package com.avsystem.commons
package mongo

import java.util.NoSuchElementException

import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization.{FieldInput, GenCodec, InputType, ListInput, ObjectInput}
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

  override def hasNext: Boolean = it.hasNext
  override def nextField(): BsonReaderFieldInput = it.next()
  private[mongo] def peek(): BsonReaderFieldInput = it.peek()
}

object BsonReaderObjectInput {
  def startReading(br: BsonReader): BsonReaderObjectInput = {
    br.readStartDocument()
    new BsonReaderObjectInput(br)
  }
}

/**
  * Modifies iteration order - moves "_id" field to last position if it was first.
  * This is a workaround for [[MongoWithIdPolymorphicGenCodec]] where objType needs to be read first.
  */
private[commons] class ObjectWithIdBsonInput[ID](objectInput: BsonReaderObjectInput)(implicit idCodec: GenCodec[ID]) extends ObjectInput {
  private val id: Opt[ID] =
    if (objectInput.hasNext && objectInput.peek().fieldName == "_id") Opt.some(idCodec.read(objectInput.nextField()))
    else Opt.empty

  private var idToRead = id.isDefined

  override def hasNext: Boolean = objectInput.hasNext || idToRead
  override def nextField(): FieldInput =
    if (objectInput.hasNext) {
      objectInput.nextField()
    }
    else if (idToRead) {
      idToRead = false
      new BsonValueFieldInput("_id", id.get)
    } else {
      throw new NoSuchElementException
    }
}

private[commons] class BsonValueFieldInput[T](name: String, value: T) extends BsonInput with FieldInput {
  override def fieldName: String = name
  override def inputType: InputType = InputType.Simple

  override def readNull(): Null = read[Null]
  override def readString(): String = read[String]
  override def readBoolean(): Boolean = read[Boolean]
  override def readInt(): Int = read[Int]
  override def readLong(): Long = read[Long]
  override def readDouble(): Double = read[Double]
  override def readBinary(): Array[Byte] = read[Array[Byte]]
  override def readList(): ListInput = read[ListInput]
  override def readObjectId(): ObjectId = read[ObjectId]

  override def readObject(): ObjectInput = throw new UnsupportedOperationException

  override def skip(): Unit = ()

  private def read[V](implicit ct: ClassTag[V]) = value match {
    case ct(_) => value.asInstanceOf[V]
    case _ => throw new ReadFailure(s"Incompatible type. Expected: ${ct.runtimeClass.getSimpleName}, actual: ${value.getClass.getSimpleName}")
  }
}