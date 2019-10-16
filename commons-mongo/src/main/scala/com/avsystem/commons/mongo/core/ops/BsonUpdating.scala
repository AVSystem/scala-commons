package com.avsystem.commons
package mongo.core.ops

import com.mongodb.client.model.Updates
import org.bson.conversions.Bson

class BsonUpdating(val bson: Bson) extends AnyVal {
  def combine(others: Bson*): Bson = Updates.combine((bson +: others).to(JList))
}
