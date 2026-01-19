package com.avsystem.commons
package mongo.core.ops

import com.avsystem.commons.mongo.core.ops.BsonEquality.bsonEquality
import com.avsystem.commons.mongo.{BsonCodec, BsonRef, DocKey}
import com.avsystem.commons.serialization.GenCodec
import com.mongodb.client.model.Sorts
import org.scalatest.funsuite.AnyFunSuite

class SortingTest extends AnyFunSuite {

  import Sorting._

  val someKey = "someKey"
  val otherKey = "otherKey"

  test("docKey tests") {
    val someDocKey = DocKey(someKey, BsonCodec.string)
    val otherDocKey = DocKey(otherKey, BsonCodec.int32)

    assert(someDocKey.ascending === Sorts.ascending(someKey))
    assert(someDocKey.descending === Sorts.descending(someKey))
    assert(someDocKey.metaTextScore === Sorts.metaTextScore(someKey))

    assert(ascending(someDocKey, otherDocKey) === Sorts.ascending(someKey, otherKey))
    assert(descending(someDocKey, otherDocKey) === Sorts.descending(someKey, otherKey))
    assert(
      orderBy(someDocKey.ascending, otherDocKey.descending) ===
        Sorts.orderBy(Sorts.ascending(someKey), Sorts.descending(otherKey))
    )
  }

  test("bsonRef tests") {
    val someRef = BsonRef[Any, String](someKey, GenCodec.StringCodec, null)
    val otherRef = BsonRef[Any, Int](otherKey, GenCodec.IntCodec, null)

    assert(someRef.ascending === Sorts.ascending(someKey))
    assert(someRef.descending === Sorts.descending(someKey))
    assert(someRef.metaTextScore === Sorts.metaTextScore(someKey))

    assert(ascending(someRef, otherRef) === Sorts.ascending(someKey, otherKey))
    assert(descending(someRef, otherRef) === Sorts.descending(someKey, otherKey))
    assert(
      orderBy(someRef.ascending, otherRef.descending) ===
        Sorts.orderBy(Sorts.ascending(someKey), Sorts.descending(otherKey))
    )
  }
}
