package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec}
import org.bson.types.ObjectId

trait BsonGenCodecs {
  implicit def objectIdCodec: GenCodec[ObjectId] = BsonGenCodecs.objectIdCodec
  implicit def objectIdKeyCodec: GenKeyCodec[ObjectId] = BsonGenCodecs.objectIdKeyCodec
}

object BsonGenCodecs {
  implicit val objectIdCodec: GenCodec[ObjectId] = GenCodec.createNullable(
    readFun = {
      case bsonInput: BsonInput => bsonInput.readObjectId()
      case otherInput => new ObjectId(otherInput.readString())
    },
    writeFun = {
      case (bsonOutput: BsonOutput, objectId) => bsonOutput.writeObjectId(objectId)
      case (otherOutput, objectId) => otherOutput.writeString(objectId.toHexString)
    }
  )
  implicit val objectIdKeyCodec: GenKeyCodec[ObjectId] =
    GenKeyCodec.create(new ObjectId(_), _.toHexString)
}
