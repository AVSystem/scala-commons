package com.avsystem.commons
package mongo.core.ops

import com.avsystem.commons.mongo.{BsonRef, BsonValueOutput}
import com.avsystem.commons.serialization.GenCodec
import org.bson.BsonValue

class BsonRefIterableUpdating[E, C[T] <: Iterable[T]](val bsonRef: BsonRef[C[E]])(implicit eCodec: GenCodec[E])
  extends BaseIterableUpdating[E, C]
    with BsonRefKeyValueHandling[C[E]] {

  override protected def encodeElement(e: E): BsonValue = {
    var result: BsonValue = null
    eCodec.write(new BsonValueOutput(result = _), e)
    result
  }
}
