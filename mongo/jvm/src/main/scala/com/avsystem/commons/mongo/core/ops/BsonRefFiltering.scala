package com.avsystem.commons
package mongo.core.ops

import com.avsystem.commons.mongo.BsonRef

final class BsonRefFiltering[T](protected val bsonRef: BsonRef[_, T])
  extends AnyVal with BaseFiltering[T] with BsonRefKeyValueHandling[T]
