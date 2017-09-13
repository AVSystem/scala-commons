package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization.GenCodec
import org.bson.types.ObjectId

trait BsonGenCodecs {
  implicit def objectIdCodec: GenCodec[ObjectId] = BsonGenCodecs.objectIdCodec
}

object BsonGenCodecs {
  implicit val objectIdCodec: GenCodec[ObjectId] = GenCodec.createNullSafe(
    readFun = {
      case bsonInput: BsonInput => bsonInput.readObjectId()
      case otherInput => new ObjectId(otherInput.readString())
    },
    writeFun = {
      case (bsonOutput: BsonOutput, objectId) => bsonOutput.writeObjectId(objectId)
      case (otherOutput, objectId) => otherOutput.writeString(objectId.toHexString)
    },
    allowNull = true
  )
}
