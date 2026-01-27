package com.avsystem.commons
package mongo.core.ops

import com.mongodb.client.model.Updates
import org.bson.conversions.Bson

object Updating {
  given Conversion[Bson, BsonUpdating] = new BsonUpdating(_)

  def combine(updates: Bson*): Bson = Updates.combine(updates.to(JList))
}
