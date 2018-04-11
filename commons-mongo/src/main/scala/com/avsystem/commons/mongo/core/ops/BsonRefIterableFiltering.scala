package com.avsystem.commons
package mongo.core.ops

import com.avsystem.commons.mongo.BsonRef
import com.avsystem.commons.serialization.GenCodec

final class BsonRefIterableFiltering[E, C[T] <: Iterable[T]](protected val bsonRef: BsonRef[_, C[E]])(
  implicit protected val elementCodec: GenCodec[E]) extends BaseIterableFiltering[E, C]
    with BsonRefKeyValueHandling[C[E]]
    with BsonRefKeyElementHandling[E, C]
