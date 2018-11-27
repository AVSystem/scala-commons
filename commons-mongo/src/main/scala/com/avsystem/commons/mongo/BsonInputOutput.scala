package com.avsystem.commons
package mongo

import java.nio.ByteBuffer

import com.avsystem.commons.serialization.{InputAndSimpleInput, InputMetadata, OutputAndSimpleOutput, TypeMarker}
import org.bson.BsonType
import org.bson.types.ObjectId

object ObjectIdMarker extends TypeMarker[ObjectId]

object BsonTypeMetadata extends InputMetadata[BsonType]

trait BsonInput extends Any with InputAndSimpleInput {
  def readObjectId(): ObjectId

  protected def bsonType: BsonType

  override def readMetadata[T](metadata: InputMetadata[T]): Opt[T] =
    metadata match {
      case BsonTypeMetadata => bsonType.opt
      case _ => Opt.Empty
    }

  override def readCustom[T](typeMarker: TypeMarker[T]): Opt[T] =
    typeMarker match {
      case ObjectIdMarker => readObjectId().opt
      case _ => Opt.Empty
    }
}

object BsonInput {
  def bigDecimalFromBytes(bytes: Array[Byte]): BigDecimal = {
    val buf = ByteBuffer.wrap(bytes)
    val unscaledBytes = new Array[Byte](bytes.length - Integer.BYTES)
    buf.get(unscaledBytes)
    val unscaled = BigInt(unscaledBytes)
    val scale = buf.getInt
    BigDecimal(unscaled, scale)
  }
}

trait BsonOutput extends Any with OutputAndSimpleOutput {
  def writeObjectId(objectId: ObjectId): Unit

  override def writeCustom[T](typeMarker: TypeMarker[T], value: T): Boolean =
    typeMarker match {
      case ObjectIdMarker => writeObjectId(value); true
      case _ => false
    }
}

object BsonOutput {
  def bigDecimalBytes(bigDecimal: BigDecimal): Array[Byte] = {
    val unscaledBytes = bigDecimal.bigDecimal.unscaledValue.toByteArray
    ByteBuffer.allocate(unscaledBytes.length + Integer.BYTES).put(unscaledBytes).putInt(bigDecimal.scale).array
  }
}
