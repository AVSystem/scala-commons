package com.avsystem.commons
package mongo.core.ops

import com.avsystem.commons.mongo.BsonRef
import org.bson.conversions.Bson

trait LowPrioFiltering {
  implicit def bsonRefFiltering[T](bsonRef: BsonRef[T]): BsonRefFiltering[T] = new BsonRefFiltering(bsonRef)
}
