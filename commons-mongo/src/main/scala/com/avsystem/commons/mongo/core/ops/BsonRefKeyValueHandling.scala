package com.avsystem.commons
package mongo.core.ops

import com.avsystem.commons.mongo.BsonValueOutput
import org.bson.BsonValue

trait BsonRefKeyValueHandling[T] extends Any with KeyValueHandling[T] with BsonRefKeyHandling[T] {
  override protected def encode(t: T): BsonValue = {
    var result: BsonValue = null
    bsonRef.codec.write(new BsonValueOutput(result = _), t)
    result
  }
}
