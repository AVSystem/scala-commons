package com.avsystem.commons
package mongo.core.ops

import org.bson.BsonValue
import org.bson.conversions.Bson

trait KeyValueHandling[T] extends Any with KeyHandling {
  protected def encode(t: T): BsonValue

  protected def use(t: T)(f: (String, BsonValue) => Bson): Bson = {
    f(key, encode(t))
  }
}
