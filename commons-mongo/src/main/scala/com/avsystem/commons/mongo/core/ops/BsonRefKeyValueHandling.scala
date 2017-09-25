package com.avsystem.commons
package mongo.core.ops

import com.avsystem.commons.mongo.{BsonRef, BsonValueOutput}
import org.bson.BsonValue

trait BsonRefKeyValueHandling[T] extends Any with KeyValueHandling[T] {
  protected def bsonRef: BsonRef[T]

  override protected def key: String = bsonRef.path
  override protected def encode(t: T): BsonValue = {
    var result: BsonValue = null
    bsonRef.codec.write(new BsonValueOutput(result = _), t)
    result
  }
}
