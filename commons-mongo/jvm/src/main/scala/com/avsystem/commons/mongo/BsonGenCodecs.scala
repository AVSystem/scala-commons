package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization._
import org.bson._
import org.bson.io.BasicOutputBuffer
import org.bson.types.{Decimal128, ObjectId}

import java.nio.ByteBuffer

trait BsonGenCodecs {
  implicit def objectIdCodec: GenCodec[ObjectId] = BsonGenCodecs.objectIdCodec
  implicit def objectIdKeyCodec: GenKeyCodec[ObjectId] = BsonGenCodecs.objectIdKeyCodec
  implicit def decimal128Codec: GenCodec[Decimal128] = BsonGenCodecs.decimal128Codec

  implicit def bsonArrayCodec: GenCodec[BsonArray] = BsonGenCodecs.bsonArrayCodec
  implicit def bsonBinaryCodec: GenCodec[BsonBinary] = BsonGenCodecs.bsonBinaryCodec
  implicit def bsonBooleanCodec: GenCodec[BsonBoolean] = BsonGenCodecs.bsonBooleanCodec
  implicit def bsonDateTimeCodec: GenCodec[BsonDateTime] = BsonGenCodecs.bsonDateTimeCodec
  implicit def bsonDocumentCodec: GenCodec[BsonDocument] = BsonGenCodecs.bsonDocumentCodec
  implicit def bsonDecimal128Codec: GenCodec[BsonDecimal128] = BsonGenCodecs.bsonDecimal128Codec
  implicit def bsonDoubleCodec: GenCodec[BsonDouble] = BsonGenCodecs.bsonDoubleCodec
  implicit def bsonInt32Codec: GenCodec[BsonInt32] = BsonGenCodecs.bsonInt32Codec
  implicit def bsonInt64Codec: GenCodec[BsonInt64] = BsonGenCodecs.bsonInt64Codec
  implicit def bsonNullCodec: GenCodec[BsonNull] = BsonGenCodecs.bsonNullCodec
  implicit def bsonObjectIdCodec: GenCodec[BsonObjectId] = BsonGenCodecs.bsonObjectIdCodec
  implicit def bsonStringCodec: GenCodec[BsonString] = BsonGenCodecs.bsonStringCodec
  implicit def bsonValueCodec: GenCodec[BsonValue] = BsonGenCodecs.bsonValueCodec
}

object BsonGenCodecs {
  // needed so that ObjectId can be used as ID type in AutoIdMongoEntity
  // (TransparentWrapping is used in EntityIdMode)
  implicit val objectIdIdentityWrapping: TransparentWrapping[ObjectId, ObjectId] = TransparentWrapping.identity

  implicit val objectIdCodec: GenCodec[ObjectId] = GenCodec.nullable(
    i => i.readCustom(ObjectIdMarker).getOrElse(new ObjectId(i.readSimple().readString())),
    (o, v) => if (!o.writeCustom(ObjectIdMarker, v)) o.writeSimple().writeString(v.toHexString)
  )

  implicit val objectIdKeyCodec: GenKeyCodec[ObjectId] =
    GenKeyCodec.create(new ObjectId(_), _.toHexString)

  implicit val decimal128Codec: GenCodec[Decimal128] = GenCodec.nullable(
    i => i.readCustom(Decimal128Marker).getOrElse(new Decimal128(i.readSimple().readBigDecimal().bigDecimal)),
    (o, v) => if (!o.writeCustom(Decimal128Marker, v)) o.writeSimple().writeBigDecimal(v.bigDecimalValue())
  )

  implicit val bsonValueCodec: GenCodec[BsonValue] = GenCodec.create(
    i => i.readCustom(BsonValueMarker).getOrElse {
      val reader = new BsonBinaryReader(ByteBuffer.wrap(i.readSimple().readBinary()))
      BsonValueUtils.decode(reader).asDocument().get("v")
    },
    (o, bv) => if (!o.writeCustom(BsonValueMarker, bv)) {
      val buffer = new BasicOutputBuffer()
      val writer = new BsonBinaryWriter(buffer)
      BsonValueUtils.encode(writer, new BsonDocument("v", bv))
      writer.flush()
      writer.close()
      o.writeSimple().writeBinary(buffer.toByteArray)
    }
  )

  private def bsonValueSubCodec[T <: BsonValue](fromBsonValue: BsonValue => T): GenCodec[T] =
    bsonValueCodec.transform(identity, fromBsonValue)

  implicit val bsonArrayCodec: GenCodec[BsonArray] = bsonValueSubCodec(_.asArray())
  implicit val bsonBinaryCodec: GenCodec[BsonBinary] = bsonValueSubCodec(_.asBinary())
  implicit val bsonBooleanCodec: GenCodec[BsonBoolean] = bsonValueSubCodec(_.asBoolean())
  implicit val bsonDateTimeCodec: GenCodec[BsonDateTime] = bsonValueSubCodec(_.asDateTime())
  implicit val bsonDocumentCodec: GenCodec[BsonDocument] = bsonValueSubCodec(_.asDocument())
  implicit val bsonDecimal128Codec: GenCodec[BsonDecimal128] = bsonValueSubCodec(_.asDecimal128())
  implicit val bsonDoubleCodec: GenCodec[BsonDouble] = bsonValueSubCodec(_.asDouble())
  implicit val bsonInt32Codec: GenCodec[BsonInt32] = bsonValueSubCodec(_.asInt32())
  implicit val bsonInt64Codec: GenCodec[BsonInt64] = bsonValueSubCodec(_.asInt64())

  implicit val bsonNullCodec: GenCodec[BsonNull] =
    bsonValueSubCodec { bv =>
      if (bv.isNull) BsonNull.VALUE
      else throw new ReadFailure("Input did not contain expected null value")
    }

  implicit val bsonObjectIdCodec: GenCodec[BsonObjectId] =
    objectIdCodec.transform(_.getValue, new BsonObjectId(_))

  implicit val bsonStringCodec: GenCodec[BsonString] =
    GenCodec.StringCodec.transform(_.getValue, new BsonString(_))
}
