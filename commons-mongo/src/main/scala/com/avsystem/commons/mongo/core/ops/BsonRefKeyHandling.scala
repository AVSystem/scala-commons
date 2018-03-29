package com.avsystem.commons
package mongo.core.ops

import com.avsystem.commons.mongo.BsonRef

trait BsonRefKeyHandling[T] extends Any with KeyHandling {
  protected def bsonRef: BsonRef[_, T]

  override protected def key: String = bsonRef.path
}
