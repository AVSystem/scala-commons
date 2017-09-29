package com.avsystem.commons
package mongo.core.ops

import com.avsystem.commons.mongo.BsonRef

class BsonRefSorting[T](val bsonRef: BsonRef[T])
  extends AnyVal
    with BaseSorting
    with BsonRefKeyHandling[T]
