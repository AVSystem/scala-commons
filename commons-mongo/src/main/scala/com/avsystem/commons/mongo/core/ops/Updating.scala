package com.avsystem.commons
package mongo.core.ops

import com.mongodb.client.model.Updates
import org.bson.conversions.Bson

object Updating {
  implicit def bsonUpdating(bson: Bson): BsonUpdating = new BsonUpdating(bson)

  def combine(updates: Bson*): Bson = Updates.combine(updates.to[JList])
}
