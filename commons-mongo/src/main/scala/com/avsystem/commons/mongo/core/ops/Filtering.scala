package com.avsystem.commons
package mongo.core.ops

import com.avsystem.commons.mongo.{BsonRef, DocKey}
import com.avsystem.commons.serialization.GenCodec
import com.mongodb.client.model.Filters
import org.bson.BsonValue
import org.bson.conversions.Bson

object Filtering extends LowPrioFiltering {
  implicit def bsonFiltering(bson: Bson): BsonFiltering = new BsonFiltering(bson)
  implicit def docKeyFiltering[T](docKey: DocKey[T, _ <: BsonValue]): DocKeyFiltering[T] = new DocKeyFiltering(docKey)
  implicit def bsonRefIterableFiltering[E: GenCodec, C[T] <: Iterable[T]](bsonRef: BsonRef[C[E]]): BsonRefIterableFiltering[E, C] = {
    new BsonRefIterableFiltering[E, C](bsonRef)
  }

  def and(filters: Bson*): Bson = Filters.and(filters.asJava)
  def or(filters: Bson*): Bson = Filters.or(filters.asJava)
  def nor(filters: Bson*): Bson = Filters.nor(filters.asJava)
  def not(filter: Bson): Bson = Filters.not(filter)
}
