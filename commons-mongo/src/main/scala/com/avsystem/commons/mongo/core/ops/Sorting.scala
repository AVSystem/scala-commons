package com.avsystem.commons
package mongo.core.ops

import com.avsystem.commons.mongo.{BsonRef, DocKey}
import com.mongodb.client.model.Sorts
import org.bson.BsonValue
import org.bson.conversions.Bson

object Sorting {
  implicit def bsonRefSorting[T](bsonRef: BsonRef[T]): BsonRefSorting[T] = new BsonRefSorting(bsonRef)
  implicit def docKeySorting[T](docKey: DocKey[T, _ <: BsonValue]): DocKeySorting[T] = new DocKeySorting(docKey)

  def ascending[K](keys: K*)(implicit kg: KeyGetter[K]): Bson = Sorts.ascending(keys.map(kg.keyOf).asJava)
  def descending[K](keys: K*)(implicit kg: KeyGetter[K]): Bson = Sorts.descending(keys.map(kg.keyOf).asJava)

  def orderBy(sorts: Bson*): Bson = Sorts.orderBy(sorts.asJava)
}
