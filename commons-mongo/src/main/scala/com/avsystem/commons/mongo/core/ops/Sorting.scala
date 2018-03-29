package com.avsystem.commons
package mongo.core.ops

import com.mongodb.client.model.Sorts
import org.bson.conversions.Bson

object Sorting {
  def ascending[K](keys: K*)(implicit kg: KeyGetter[K]): Bson = Sorts.ascending(keys.map(kg.keyOf).asJava)
  def descending[K](keys: K*)(implicit kg: KeyGetter[K]): Bson = Sorts.descending(keys.map(kg.keyOf).asJava)

  def orderBy(sorts: Bson*): Bson = Sorts.orderBy(sorts.asJava)
}
