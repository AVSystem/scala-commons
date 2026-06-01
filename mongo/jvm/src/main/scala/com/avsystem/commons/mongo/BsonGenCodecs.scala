package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization._
import org.bson._
import org.bson.io.BasicOutputBuffer
import org.bson.types.{Decimal128, ObjectId}

import java.nio.ByteBuffer

trait BsonGenCodecs {
  export BsonGenCodecs.given
}

object BsonGenCodecs {
  // needed so that ObjectId can be used as ID type in AutoIdMongoEntity
  // (TransparentWrapping is used in EntityIdMode)
  given TransparentWrapping[ObjectId, ObjectId] = TransparentWrapping.identity

  given GenCodec[ObjectId] = GenCodec.nullable(
    i => i.readCustom(ObjectIdMarker).getOrElse(new ObjectId(i.readSimple().readString())),
    (o, v) => if (!o.writeCustom(ObjectIdMarker, v)) o.writeSimple().writeString(v.toHexString),
  )

  given GenKeyCodec[ObjectId] =
    GenKeyCodec.create(new ObjectId(_), _.toHexString)

  given GenCodec[Decimal128] = GenCodec.nullable(
    i => i.readCustom(Decimal128Marker).getOrElse(new Decimal128(i.readSimple().readBigDecimal().bigDecimal)),
    (o, v) => if (!o.writeCustom(Decimal128Marker, v)) o.writeSimple().writeBigDecimal(v.bigDecimalValue()),
  )

  given GenCodec[BsonValue] = GenCodec.create(
    i =>
      i.readCustom(BsonValueMarker).getOrElse {
        val reader = new BsonBinaryReader(ByteBuffer.wrap(i.readSimple().readBinary()))
        BsonValueUtils.decode(reader).asDocument().get("v")
      },
    (o, bv) =>
      if (!o.writeCustom(BsonValueMarker, bv)) {
        val buffer = new BasicOutputBuffer()
        val writer = new BsonBinaryWriter(buffer)
        BsonValueUtils.encode(writer, new BsonDocument("v", bv))
        writer.flush()
        writer.close()
        o.writeSimple().writeBinary(buffer.toByteArray)
      },
  )

  private def bsonValueSubCodec[T <: BsonValue](fromBsonValue: BsonValue => T): GenCodec[T] =
    summon[GenCodec[BsonValue]].transform(identity, fromBsonValue)

  given GenCodec[BsonArray] = bsonValueSubCodec(_.asArray())
  given GenCodec[BsonBinary] = bsonValueSubCodec(_.asBinary())
  given GenCodec[BsonBoolean] = bsonValueSubCodec(_.asBoolean())
  given GenCodec[BsonDateTime] = bsonValueSubCodec(_.asDateTime())
  given GenCodec[BsonDocument] = bsonValueSubCodec(_.asDocument())
  given GenCodec[BsonDecimal128] = bsonValueSubCodec(_.asDecimal128())
  given GenCodec[BsonDouble] = bsonValueSubCodec(_.asDouble())
  given GenCodec[BsonInt32] = bsonValueSubCodec(_.asInt32())
  given GenCodec[BsonInt64] = bsonValueSubCodec(_.asInt64())

  given GenCodec[BsonNull] =
    bsonValueSubCodec { bv =>
      if (bv.isNull) BsonNull.VALUE
      else throw new ReadFailure("Input did not contain expected null value")
    }

  given GenCodec[BsonObjectId] =
    summon[GenCodec[ObjectId]].transform(_.getValue, new BsonObjectId(_))

  given GenCodec[BsonString] =
    GenCodec.StringCodec.transform(_.getValue, new BsonString(_))

  // Source-compat aliases for callers that previously referenced these by name.
  @deprecated("Use summon[TransparentWrapping[ObjectId, ObjectId]]", since = "scala-3")
  def objectIdIdentityWrapping: TransparentWrapping[ObjectId, ObjectId] = summon
  @deprecated("Use summon[GenCodec[ObjectId]]", since = "scala-3")
  def objectIdCodec: GenCodec[ObjectId] = summon
  @deprecated("Use summon[GenKeyCodec[ObjectId]]", since = "scala-3")
  def objectIdKeyCodec: GenKeyCodec[ObjectId] = summon
  @deprecated("Use summon[GenCodec[Decimal128]]", since = "scala-3")
  def decimal128Codec: GenCodec[Decimal128] = summon
  @deprecated("Use summon[GenCodec[BsonValue]]", since = "scala-3")
  def bsonValueCodec: GenCodec[BsonValue] = summon
  @deprecated("Use summon[GenCodec[BsonArray]]", since = "scala-3")
  def bsonArrayCodec: GenCodec[BsonArray] = summon
  @deprecated("Use summon[GenCodec[BsonBinary]]", since = "scala-3")
  def bsonBinaryCodec: GenCodec[BsonBinary] = summon
  @deprecated("Use summon[GenCodec[BsonBoolean]]", since = "scala-3")
  def bsonBooleanCodec: GenCodec[BsonBoolean] = summon
  @deprecated("Use summon[GenCodec[BsonDateTime]]", since = "scala-3")
  def bsonDateTimeCodec: GenCodec[BsonDateTime] = summon
  @deprecated("Use summon[GenCodec[BsonDocument]]", since = "scala-3")
  def bsonDocumentCodec: GenCodec[BsonDocument] = summon
  @deprecated("Use summon[GenCodec[BsonDecimal128]]", since = "scala-3")
  def bsonDecimal128Codec: GenCodec[BsonDecimal128] = summon
  @deprecated("Use summon[GenCodec[BsonDouble]]", since = "scala-3")
  def bsonDoubleCodec: GenCodec[BsonDouble] = summon
  @deprecated("Use summon[GenCodec[BsonInt32]]", since = "scala-3")
  def bsonInt32Codec: GenCodec[BsonInt32] = summon
  @deprecated("Use summon[GenCodec[BsonInt64]]", since = "scala-3")
  def bsonInt64Codec: GenCodec[BsonInt64] = summon
  @deprecated("Use summon[GenCodec[BsonNull]]", since = "scala-3")
  def bsonNullCodec: GenCodec[BsonNull] = summon
  @deprecated("Use summon[GenCodec[BsonObjectId]]", since = "scala-3")
  def bsonObjectIdCodec: GenCodec[BsonObjectId] = summon
  @deprecated("Use summon[GenCodec[BsonString]]", since = "scala-3")
  def bsonStringCodec: GenCodec[BsonString] = summon
}
