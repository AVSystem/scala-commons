package com.avsystem.commons
package mongo.core.ops

import com.avsystem.commons.mongo.DocKey
import org.mongodb.scala.bson.BsonValue

class DocKeyFiltering[T](val docKey: DocKey[T, _ <: BsonValue]) extends AnyVal with BaseFiltering[T] {
  override private[core] def key = docKey.key
  override private[core] def encode(t: T) = docKey.codec.toBson(t)
}
