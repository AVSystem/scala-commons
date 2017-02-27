package com.avsystem.commons
package mongo

import com.avsystem.commons.mongo.Filter.Limitations.CanCompare
import org.bson.conversions.Bson
import org.bson.{BsonString, BsonValue}

object Filtering {
  implicit class DocKeyOps[A](val key: DocKey[A, _]) extends AnyVal {
    def equal(value: A): Bson = Filter.equal(key, value)
    def notEqual(value: A): Bson = Filter.notEqual(key, value)

    def in(values: A*): Bson = Filter.in(key, values)
    def nin(values: A*): Bson = Filter.nin(key, values)

    def exists: Bson = Filter.exists(key)
    def notExists: Bson = Filter.notExists(key)
  }

  implicit class CanCompareDocKeyOps[A, BSON <: BsonValue](val key: DocKey[A, BSON]) extends AnyVal {
    def lt(value: A)(implicit cc: CanCompare[BSON]): Bson = Filter.lt(key, value)
    def lte(value: A)(implicit cc: CanCompare[BSON]): Bson = Filter.lte(key, value)
    def gt(value: A)(implicit cc: CanCompare[BSON]): Bson = Filter.gt(key, value)
    def gte(value: A)(implicit cc: CanCompare[BSON]): Bson = Filter.gte(key, value)
  }

  implicit class StringKeyOps[A](val key: DocKey[A, BsonString]) extends AnyVal {
    def regex(pattern: String): Bson = Filter.regex(key, pattern)
  }

  implicit class BsonOps(val bson: Bson) extends AnyVal {
    def and(others: Bson*): Bson = Filter.and(bson +: others: _*)
  }
}
