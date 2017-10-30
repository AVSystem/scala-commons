package com.avsystem.commons
package mongo.core.ops

import com.mongodb.client.model.Filters
import org.bson.conversions.Bson

trait BaseIterableFiltering[E, C[T] <: Iterable[T]] extends Any with BaseFiltering[C[E]] with KeyElementHandling[E] {
  def contains(e: E): Bson = useE(e)(Filters.eq(_, _))
  def all(es: E*): Bson = useEs(es)(Filters.all(_, _))
  def elemMatch(filter: Bson): Bson = Filters.elemMatch(key, filter)
  def size(size: Int): Bson = Filters.size(key, size)
}
