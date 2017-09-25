package com.avsystem.commons
package mongo.core.ops

import com.avsystem.commons.mongo.DocKey
import org.bson.BsonValue

trait DocKeyKeyValueHandling[T] extends Any with KeyValueHandling[T] {
  protected def docKey: DocKey[T, _ <: BsonValue]

  override protected def key: String = docKey.key
  override protected def encode(t: T): BsonValue = docKey.codec.toBson(t)
}
