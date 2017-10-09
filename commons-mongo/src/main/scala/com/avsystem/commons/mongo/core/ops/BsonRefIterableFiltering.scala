package com.avsystem.commons
package mongo.core.ops

import com.avsystem.commons.mongo.BsonRef
import com.avsystem.commons.serialization.GenCodec

class BsonRefIterableFiltering[E, C[T] <: Iterable[T]](val bsonRef: BsonRef[C[E]])(implicit val elementCodec: GenCodec[E])
  extends BaseIterableFiltering[E, C]
    with BsonRefKeyValueHandling[C[E]]
    with BsonRefKeyElementHandling[E, C]
