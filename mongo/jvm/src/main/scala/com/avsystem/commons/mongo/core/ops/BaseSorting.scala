package com.avsystem.commons
package mongo.core.ops

import com.mongodb.client.model.Sorts
import org.bson.conversions.Bson

trait BaseSorting extends Any with KeyHandling {
  def ascending: Bson = Sorts.ascending(key)
  def descending: Bson = Sorts.descending(key)
  def metaTextScore: Bson = Sorts.metaTextScore(key)
}
