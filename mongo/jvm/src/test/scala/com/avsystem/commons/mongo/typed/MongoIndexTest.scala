package com.avsystem.commons
package mongo.typed

import org.scalatest.funsuite.AnyFunSuite

class MongoIndexTest extends AnyFunSuite {
  final val Rte = RecordTestEntity
  final val Ute = UnionTestEntity
  final val Ir = InnerRecord

  test("index bsons") {
    assert(Rte.ref(_.int).ascendingIndex.toBson.toString == """{"int": 1}""")
    assert(Rte.ref(_.int).descendingIndex.toBson.toString == """{"int": -1}""")
    assert(MongoIndex.ascending(Rte.ref(_.int), Rte.ref(_.renamedStr)).toBson.toString == """{"int": 1, "str": 1}""")
    assert(MongoIndex.descending(Rte.ref(_.int), Rte.ref(_.renamedStr)).toBson.toString == """{"int": -1, "str": -1}""")

    assert(Rte.ref(_.int).index(MongoIndexType.Hashed).toBson.toString == """{"int": "hashed"}""")
  }
}
