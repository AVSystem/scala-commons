package com.avsystem.commons
package mongo.core.ops

import com.mongodb.client.model.Filters
import org.bson.conversions.Bson

object Filtering {
  given Conversion[Bson, BsonFiltering] = new BsonFiltering(_)

  def and(filters: Bson*): Bson = Filters.and(filters.asJava)
  def or(filters: Bson*): Bson = Filters.or(filters.asJava)
  def nor(filters: Bson*): Bson = Filters.nor(filters.asJava)
  def not(filter: Bson): Bson = Filters.not(filter)
}
