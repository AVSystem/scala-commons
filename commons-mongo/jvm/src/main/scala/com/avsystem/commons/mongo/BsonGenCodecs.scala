package com.avsystem.commons
package mongo

import java.nio.ByteBuffer
import com.avsystem.commons.serialization.GenCodec.{ReadFailure, WriteFailure}
import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec, TransparentWrapping}
import org.bson.codecs.{BsonValueCodec, DecoderContext, EncoderContext}
import org.bson.io.BasicOutputBuffer
import org.bson.types.{Decimal128, ObjectId}
import org.bson.{BsonArray, BsonBinary, BsonBinaryReader, BsonBinaryWriter, BsonBoolean, BsonDateTime, BsonDecimal128, BsonDocument, BsonDouble, BsonElement, BsonInt32, BsonInt64, BsonNull, BsonObjectId, BsonString, BsonType, BsonValue}

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
  implicit val objectIdIdentityWrapping: TransparentWrapping[ObjectId, ObjectId] =
    TransparentWrapping.identity

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

  implicit val bsonArrayCodec: GenCodec[BsonArray] = GenCodec.nullableList(
    li => new BsonArray(li.iterator(bsonValueCodec.read).to(JList)),
    (lo, ba) => ba.asScala.foreach(bsonValueCodec.write(lo.writeElement(), _))
  )

  implicit val bsonBinaryCodec: GenCodec[BsonBinary] =
    GenCodec.ByteArrayCodec.transform[BsonBinary](_.getData, new BsonBinary(_))

  implicit val bsonBooleanCodec: GenCodec[BsonBoolean] =
    GenCodec.BooleanCodec.transform[BsonBoolean](_.getValue, new BsonBoolean(_))

  implicit val bsonDateTimeCodec: GenCodec[BsonDateTime] = GenCodec.nullableSimple(
    i => new BsonDateTime(i.readTimestamp()),
    (o, v) => o.writeTimestamp(v.getValue)
  )

  implicit val bsonDocumentCodec: GenCodec[BsonDocument] = GenCodec.nullableObject(
    oi => new BsonDocument(oi.iterator(bsonValueCodec.read).map {
      case (k, v) => new BsonElement(k, v)
    }.to(JList)),
    (oo, bd) => bd.asScala.foreach { case (key, value) =>
      bsonValueCodec.write(oo.writeField(key), value)
    }
  )

  implicit val bsonDecimal128Codec: GenCodec[BsonDecimal128] =
    decimal128Codec.transform[BsonDecimal128](_.getValue, new BsonDecimal128(_))

  implicit val bsonDoubleCodec: GenCodec[BsonDouble] =
    GenCodec.DoubleCodec.transform(_.getValue, new BsonDouble(_))

  implicit val bsonInt32Codec: GenCodec[BsonInt32] =
    GenCodec.IntCodec.transform(_.getValue, new BsonInt32(_))

  implicit val bsonInt64Codec: GenCodec[BsonInt64] =
    GenCodec.LongCodec.transform(_.getValue, new BsonInt64(_))

  implicit val bsonNullCodec: GenCodec[BsonNull] =
    GenCodec.create(i => {
      if (!i.readNull()) throw new ReadFailure("Input did not contain expected null value")
      BsonNull.VALUE
    }, (o, _) => o.writeNull())

  implicit val bsonObjectIdCodec: GenCodec[BsonObjectId] =
    objectIdCodec.transform(_.getValue, new BsonObjectId(_))

  implicit val bsonStringCodec: GenCodec[BsonString] =
    GenCodec.StringCodec.transform(_.getValue, new BsonString(_))

  private val mongoBsonValueCodec: BsonValueCodec = new BsonValueCodec()
  private val mongoDecoderContext: DecoderContext = DecoderContext.builder().build()
  private val mongoEncoderContext: EncoderContext = EncoderContext.builder().build()

  implicit val bsonValueCodec: GenCodec[BsonValue] = GenCodec.create(
    i => {
      val bvOpt = i.readMetadata(BsonTypeMetadata) map {
        case BsonType.ARRAY => bsonArrayCodec.read(i)
        case BsonType.BINARY => bsonBinaryCodec.read(i)
        case BsonType.BOOLEAN => bsonBooleanCodec.read(i)
        case BsonType.DATE_TIME => bsonDateTimeCodec.read(i)
        case BsonType.DECIMAL128 => bsonDecimal128Codec.read(i)
        case BsonType.DOCUMENT => bsonDocumentCodec.read(i)
        case BsonType.DOUBLE => bsonDoubleCodec.read(i)
        case BsonType.INT32 => bsonInt32Codec.read(i)
        case BsonType.INT64 => bsonInt64Codec.read(i)
        case BsonType.NULL => bsonNullCodec.read(i)
        case BsonType.OBJECT_ID => bsonObjectIdCodec.read(i)
        case BsonType.STRING => bsonStringCodec.read(i)
        case other => throw new ReadFailure(s"Unsupported Bson type: $other")
      }
      bvOpt.getOrElse {
        val reader = new BsonBinaryReader(ByteBuffer.wrap(i.readSimple().readBinary()))
        mongoBsonValueCodec.decode(reader, mongoDecoderContext)
      }
    },
    (o, bv) => if (o.keepsMetadata(BsonTypeMetadata)) {
      bv match {
        case array: BsonArray => bsonArrayCodec.write(o, array)
        case binary: BsonBinary => bsonBinaryCodec.write(o, binary)
        case boolean: BsonBoolean => bsonBooleanCodec.write(o, boolean)
        case dateTime: BsonDateTime => bsonDateTimeCodec.write(o, dateTime)
        case decimal128: BsonDecimal128 => bsonDecimal128Codec.write(o, decimal128)
        case document: BsonDocument => bsonDocumentCodec.write(o, document)
        case double: BsonDouble => bsonDoubleCodec.write(o, double)
        case int32: BsonInt32 => bsonInt32Codec.write(o, int32)
        case int64: BsonInt64 => bsonInt64Codec.write(o, int64)
        case bNull: BsonNull => bsonNullCodec.write(o, bNull)
        case objectId: BsonObjectId => bsonObjectIdCodec.write(o, objectId)
        case string: BsonString => bsonStringCodec.write(o, string)
        case other => throw new WriteFailure(s"Unsupported value: $other")
      }
    } else {
      val buffer = new BasicOutputBuffer()
      val writer = new BsonBinaryWriter(buffer)
      mongoBsonValueCodec.encode(writer, bv, mongoEncoderContext)
      writer.flush()
      writer.close()
      o.writeSimple().writeBinary(buffer.toByteArray)
    }
  )
}
