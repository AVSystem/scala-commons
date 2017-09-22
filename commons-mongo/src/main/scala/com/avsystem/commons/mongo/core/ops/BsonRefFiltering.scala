package com.avsystem.commons
package mongo.core.ops

import com.avsystem.commons.mongo.{BsonRef, BsonValueOutput}
import org.bson.BsonValue

class BsonRefFiltering[T](val bsonRef: BsonRef[T]) extends AnyVal with BaseFiltering[T] {
  override private[core] def key = bsonRef.path
  override private[core] def encode(t: T) = {
    var result: BsonValue = null
    bsonRef.codec.write(new BsonValueOutput(result = _), t)
    result
  }
}
