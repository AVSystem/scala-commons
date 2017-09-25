package com.avsystem.commons
package mongo.core.ops

import com.avsystem.commons.mongo.{BsonRef, DocKey}

trait LowPrioUpdating {
  def bsonRefUpdating[T](bsonRef: BsonRef[T]): BsonRefUpdating[T] = new BsonRefUpdating(bsonRef)
}
