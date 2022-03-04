package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization.{InputAndSimpleInput, InputMetadata, OutputAndSimpleOutput, TypeMarker}
import org.bson.{BsonInvalidOperationException, BsonType, BsonValue}
import org.bson.types.{Decimal128, ObjectId}

import java.nio.ByteBuffer

object ObjectIdMarker extends TypeMarker[ObjectId]
object Decimal128Marker extends TypeMarker[Decimal128]
object BsonValueMarker extends TypeMarker[BsonValue]

object BsonTypeMetadata extends InputMetadata[BsonType]

trait BsonInput extends Any with InputAndSimpleInput {
  def readObjectId(): ObjectId
  def readDecimal128(): Decimal128
  def readBsonValue(): BsonValue

  protected def bsonType: BsonType

  protected def handleFailures[T](expr: => T): T =
    try expr catch {
      case e: BsonInvalidOperationException => throw new ReadFailure(e.getMessage, e)
    }

  protected def wrongType(expected: BsonType*): Nothing =
    throw new ReadFailure(s"Encountered BsonValue of type $bsonType, expected ${expected.mkString(" or ")}")

  protected def expect[T](tpe: BsonType, value: => T): T =
    if (bsonType == tpe) handleFailures(value)
    else wrongType(tpe)

  override def readBigInt(): BigInt = handleFailures {
    bsonType match {
      case BsonType.INT32 | BsonType.INT64 =>
        BigInt(readLong())
      case BsonType.DECIMAL128 =>
        val bigDec = BigDecimal(readDecimal128().bigDecimalValue())
        bigDec.toBigIntExact.getOrElse(throw new ReadFailure(s"whole number expected but got $bigDec"))
      case BsonType.BINARY =>
        BigInt(readBinary())
      case _ =>
        wrongType(BsonType.INT32, BsonType.INT64, BsonType.DECIMAL128, BsonType.BINARY)
    }
  }

  override def readBigDecimal(): BigDecimal =
    handleFailures {
      bsonType match {
        case BsonType.INT32 | BsonType.INT64 =>
          BigDecimal(readLong())
        case BsonType.DOUBLE =>
          BigDecimal(readDouble())
        case BsonType.DECIMAL128 =>
          BigDecimal(readDecimal128().bigDecimalValue)
        case BsonType.BINARY =>
          BsonInput.bigDecimalFromBytes(readBinary())
        case _ =>
          wrongType(BsonType.INT32, BsonType.INT64, BsonType.DOUBLE, BsonType.DECIMAL128, BsonType.BINARY)
      }
    }

  override def readMetadata[T](metadata: InputMetadata[T]): Opt[T] =
    metadata match {
      case BsonTypeMetadata => bsonType.opt
      case _ => Opt.Empty
    }

  override def readCustom[T](typeMarker: TypeMarker[T]): Opt[T] =
    typeMarker match {
      case ObjectIdMarker => readObjectId().opt
      case Decimal128Marker => readDecimal128().opt
      case BsonValueMarker => readBsonValue().opt
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
  def writeDecimal128(decimal128: Decimal128): Unit
  def writeBsonValue(bsonValue: BsonValue): Unit

  override def writeBigInt(bigInt: BigInt): Unit =
    if (bigInt.isValidLong) writeLong(bigInt.longValue)
    else Decimal128Utils.fromBigDecimal(BigDecimal(bigInt)) match {
      case Opt(dec128) => writeDecimal128(dec128)
      case Opt.Empty => writeBinary(bigInt.toByteArray)
    }

  override def writeBigDecimal(bigDecimal: BigDecimal): Unit =
    Decimal128Utils.fromBigDecimal(bigDecimal) match {
      case Opt(dec128) => writeDecimal128(dec128)
      case Opt.Empty => writeBinary(BsonOutput.bigDecimalBytes(bigDecimal))
    }

  override def keepsMetadata(metadata: InputMetadata[_]): Boolean =
    BsonTypeMetadata == metadata

  override def writeCustom[T](typeMarker: TypeMarker[T], value: T): Boolean =
    typeMarker match {
      case ObjectIdMarker => writeObjectId(value); true
      case Decimal128Marker => writeDecimal128(value); true
      case BsonValueMarker => writeBsonValue(value); true
      case _ => false
    }
}

object BsonOutput {
  def bigDecimalBytes(bigDecimal: BigDecimal): Array[Byte] = {
    val unscaledBytes = bigDecimal.bigDecimal.unscaledValue.toByteArray
    ByteBuffer.allocate(unscaledBytes.length + Integer.BYTES)
      .put(unscaledBytes).putInt(bigDecimal.scale).array
  }
}
