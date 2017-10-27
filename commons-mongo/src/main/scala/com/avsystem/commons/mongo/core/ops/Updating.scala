package com.avsystem.commons
package mongo.core.ops

import com.avsystem.commons.mongo.BsonRef
import com.avsystem.commons.serialization.GenCodec
import com.mongodb.client.model.Updates
import org.bson.conversions.Bson

object Updating extends LowPrioUpdating {
  implicit def bsonUpdating(bson: Bson): BsonUpdating = new BsonUpdating(bson)
  implicit def bsonRefIterableUpdating[E: GenCodec, C[T] <: Iterable[T]](bsonRef: BsonRef[C[E]]): BsonRefIterableUpdating[E, C] = {
    new BsonRefIterableUpdating[E, C](bsonRef)
  }

  def combine(updates: Bson*): Bson = Updates.combine(updates.to[JList])
}
