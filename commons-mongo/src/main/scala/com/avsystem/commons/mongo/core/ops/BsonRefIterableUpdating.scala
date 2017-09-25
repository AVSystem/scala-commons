package com.avsystem.commons
package mongo.core.ops

import com.avsystem.commons.mongo.{BsonRef, BsonValueOutput}
import com.avsystem.commons.serialization.GenCodec
import org.bson.BsonValue

class BsonRefIterableUpdating[E, T <: Iterable[E]](val bsonRef: BsonRef[T])(implicit eCodec: GenCodec[E])
  extends BaseIterableUpdating[E, T]
    with BsonRefKeyValueHandling[T] {

  override protected def encodeElement(e: E): BsonValue = {
    var result: BsonValue = null
    eCodec.write(new BsonValueOutput(result = _), e)
    result
  }
}
