package com.avsystem.commons
package mongo.typed

import org.scalatest.funsuite.AnyFunSuite

class MongoOrderTest extends AnyFunSuite {
  final val Rte = RecordTestEntity
  final val Ute = UnionTestEntity
  final val Ir = InnerRecord

  test("simple") {
    assert(MongoOrder.ascending[Int].toBson.toString == "BsonInt32{value=1}")
    assert(MongoOrder.descending[Int].toBson.toString == "BsonInt32{value=-1}")
  }

  test("empty") {
    assert(MongoDocumentOrder.unspecified[InnerRecord].toBson.toString == "{}")
  }

  test("single field") {
    assert(Rte.ref(_.int).ascending.toBson.toString ==
      """{"int": 1}""")
    assert(Rte.ref(_.int).descending.toBson.toString ==
      """{"int": -1}""")
  }

  test("multi field") {
    assert(Rte.ref(_.int).ascending.andThenDescendingBy(Rte.ref(_.renamedStr)).toBson.toString ==
      """{"int": 1, "str": -1}""")
    assert(MongoDocumentOrder(Rte.ref(_.int) -> true, Rte.ref(_.renamedStr) -> false).toBson.toString ==
      """{"int": 1, "str": -1}""")
  }
}
