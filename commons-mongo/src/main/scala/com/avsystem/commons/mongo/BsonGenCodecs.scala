package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec}
import org.bson.types.ObjectId

trait BsonGenCodecs {
  implicit def objectIdCodec: GenCodec[ObjectId] = BsonGenCodecs.objectIdCodec
  implicit def objectIdKeyCodec: GenKeyCodec[ObjectId] = BsonGenCodecs.objectIdKeyCodec
}

object BsonGenCodecs {
  implicit val objectIdCodec: GenCodec[ObjectId] = GenCodec.nullableSimple(
    i => i.readCustom(ObjectIdMarker).getOrElse(new ObjectId(i.readString())),
    (o, v) => if (!o.writeCustom(ObjectIdMarker, v)) o.writeString(v.toHexString)
  )
  implicit val objectIdKeyCodec: GenKeyCodec[ObjectId] =
    GenKeyCodec.create(new ObjectId(_), _.toHexString)
}
