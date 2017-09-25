package com.avsystem.commons
package mongo.core.ops

import org.bson.BsonValue
import org.bson.conversions.Bson

trait KeyValueHandling[T] extends Any {
  protected def key: String
  protected def encode(t: T): BsonValue

  protected def use(t: T)(f: (String, BsonValue) => Bson): Bson = {
    f(key, encode(t))
  }
}
