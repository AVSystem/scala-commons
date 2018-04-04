package com.avsystem.commons
package mongo.core.ops

import com.mongodb.client.model.Filters
import org.bson.conversions.Bson

final class BsonFiltering(private val bson: Bson) extends AnyVal {
  def and(others: Bson*): Bson = Filters.and((bson +: others).asJava)
  def or(others: Bson*): Bson = Filters.or((bson +: others).asJava)
  def nor(others: Bson*): Bson = Filters.nor((bson +: others).asJava)
  def not: Bson = Filters.not(bson)
}
