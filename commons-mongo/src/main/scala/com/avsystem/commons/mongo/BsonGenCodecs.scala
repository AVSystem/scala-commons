package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization.GenCodec
import org.bson.types.ObjectId

trait BsonGenCodecs {
  implicit def objectIdCodec: GenCodec[ObjectId] = BsonGenCodecs.objectIdCodec
}

object BsonGenCodecs {
  implicit val objectIdCodec: GenCodec[ObjectId] = GenCodec.createNullSafe(
    readFun = _.asInstanceOf[BsonInput].readObjectId(),
    writeFun = _.asInstanceOf[BsonOutput].writeObjectId(_),
    allowNull = true
  )
}
