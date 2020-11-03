package com.avsystem.commons
package mongo.typed

import org.scalatest.funsuite.AnyFunSuite

class MongoProjectionTest extends AnyFunSuite {
  final val Rte = RecordTestEntity
  final val Ute = UnionTestEntity
  final val Ir = InnerRecord

  import UnionTestEntity._

  def bsonStr(proj: MongoProjection[_, _]): String =
    proj.toProjectionBson.toString

  test("empty") {
    assert(bsonStr(Rte.SelfRef) == """{}""")
    assert(bsonStr(Ute.as[HasInner]) == """{}""")
  }

  test("single field") {
    assert(bsonStr(Rte.IdRef) == """{"_id": 1}""")
    assert(bsonStr(Rte.ref(_.int)) == """{"int": 1, "_id": 0}""")
    assert(bsonStr(Rte.ref(_.inner.int)) == """{"inner.int": 1, "_id": 0}""")
    assert(bsonStr(Rte.ref(_.intList.head)) == """{"intList": 1, "_id": 0}""")
  }

  test("multi field") {
    assert(bsonStr(Rte.IdRef zip Rte.ref(_.int)) == """{"_id": 1, "int": 1}""")
    assert(bsonStr(MongoProjection.zip(Rte.IdRef, Rte.ref(_.int))) == """{"_id": 1, "int": 1}""")
    assert(bsonStr(MongoProjection.zip(Rte.SelfRef, Rte.ref(_.int))) == """{}""")
  }

  test("reading bson") {
    val rteBson = Rte.format.writeBson(Rte.Example).asDocument
    assert(!rteBson.containsKey("intOpt"))
    assert(!rteBson.containsKey("union"))

    assert(Rte.SelfRef.decodeFrom(rteBson) == Rte.Example)
    assert(Rte.IdRef.decodeFrom(rteBson) == "rid")
    assert(Rte.ref(_.int).decodeFrom(rteBson) == 42)
    assert(Rte.ref(_.intOpt).decodeFrom(rteBson) == Opt.Empty)
    assert(Rte.ref(_.strOpt.get).decodeFrom(rteBson) == "stropt")
    assert(Rte.ref(_.inner.int).decodeFrom(rteBson) == 24)
    assert(Rte.ref(_.union.str).decodeFrom(rteBson) == "ustr")
    assert((Rte.IdRef zip Rte.ref(_.int)).decodeFrom(rteBson) == ("rid", 42))
    assert(MongoProjection.zip(Rte.IdRef, Rte.ref(_.int), Rte.ref(_.union.str)).decodeFrom(rteBson) == ("rid", 42, "ustr"))
    assert(Rte.ref(_.intList.head).decodeFrom(rteBson) == 1)
    assert(Rte.ref(_.intList).map(_.head).decodeFrom(rteBson) == 1)
    assert(Rte.ref(_.inner.intList(2)).decodeFrom(rteBson) == 5)
  }
}
