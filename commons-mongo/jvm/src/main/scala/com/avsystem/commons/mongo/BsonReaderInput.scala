package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization.{ListInput, ObjectInput}
import com.google.common.collect.AbstractIterator
import org.bson._
import org.bson.types.{Decimal128, ObjectId}

import _root_.scala.annotation.tailrec

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

  override def readLong(): Long = handleFailures {
    bsonType match {
      case BsonType.INT32 => br.readInt32().toLong
      case BsonType.INT64 => br.readInt64()
      case _ => wrongType(BsonType.INT32, BsonType.INT64)
    }
  }

  override def readTimestamp(): Long =
    expect(BsonType.DATE_TIME, br.readDateTime())

  override def readDouble(): Double =
    expect(BsonType.DOUBLE, br.readDouble())

  override def readBinary(): Array[Byte] =
    expect(BsonType.BINARY, br.readBinaryData().getData)

  override def readList(): BsonReaderListInput = handleFailures {
    br.readStartArray()
    new BsonReaderListInput(new BsonReaderIterator(br, _.readEndArray(), new BsonReaderInput(_, legacyOptionEncoding)))
  }

  override def readObject(): BsonReaderObjectInput = handleFailures {
    br.readStartDocument()
    new BsonReaderObjectInput(br, legacyOptionEncoding)
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
  extends BsonReaderInput(br, legacyOptionEncoding) with BsonFieldInput {
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

final class BsonReaderObjectInput(br: BsonReader, legacyOptionEncoding: Boolean) extends ObjectInput {
  private[this] val it = new BsonReaderIterator(br, _.readEndDocument(),
    br => new BsonReaderFieldInput(
      KeyEscaper.unescape(br.readName()),
      br,
      legacyOptionEncoding
    )
  )

  private[this] var peekMark: BsonReaderMark = br.getMark
  private[this] var peekedFields: MHashMap[String, BsonValue] = _

  override def peekField(name: String): Opt[BsonFieldInput] =
    br match {
      case _: BsonDocumentReader =>
        // Looks like there's a bug in BsonDocumentReader.Mark implementation, tests don't pass
        // TODO: find and report/fix this bug
        Opt.Empty
      case _ =>
        val peekedValue = peekedFields.opt.flatMap(_.getOpt(name)).orElse {
          val savedMark = br.getMark
          peekMark.reset()

          @tailrec def loop(): Opt[BsonValue] =
            if (br.readBsonType() == BsonType.END_OF_DOCUMENT) Opt.Empty
            else KeyEscaper.unescape(br.readName()) match {
              case `name` => BsonValueUtils.decode(br).opt
              case otherName =>
                if (peekedFields eq null) {
                  peekedFields = new MHashMap
                }
                peekedFields(otherName) = BsonValueUtils.decode(br)
                peekMark = br.getMark
                loop()
            }

          try loop() finally savedMark.reset()
        }

        peekedValue.map(new BsonValueFieldInput(name, _, legacyOptionEncoding))
    }

  override def hasNext: Boolean = it.hasNext
  override def nextField(): BsonReaderFieldInput = it.next()
}
