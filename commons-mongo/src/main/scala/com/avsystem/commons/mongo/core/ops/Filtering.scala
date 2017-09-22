package com.avsystem.commons
package mongo.core.ops

import com.avsystem.commons.mongo.{BsonRef, DocKey}
import org.bson.BsonValue
import org.bson.conversions.Bson

object Filtering {
  implicit def bsonFiltering(bson: Bson): BsonFiltering = new BsonFiltering(bson)
  implicit def docKeyFiltering[T](docKey: DocKey[T, _ <: BsonValue]): DocKeyFiltering[T] = new DocKeyFiltering(docKey)
  implicit def bsonRefFiltering[T](bsonRef: BsonRef[T]): BsonRefFiltering[T] = new BsonRefFiltering(bsonRef)
}
