package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec}
import org.bson.types.{Decimal128, ObjectId}

trait BsonGenCodecs {
  implicit def objectIdCodec: GenCodec[ObjectId] = BsonGenCodecs.objectIdCodec
  implicit def objectIdKeyCodec: GenKeyCodec[ObjectId] = BsonGenCodecs.objectIdKeyCodec

  implicit def decimal128Codec: GenCodec[Decimal128] = BsonGenCodecs.decimal128Codec
}

object BsonGenCodecs {
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
}
