package com.avsystem.commons
package mongo.typed

import org.scalatest.funsuite.AnyFunSuite

class MongoRefTest extends AnyFunSuite {
  final val Rte = RecordTestEntity
  final val Ute = UnionTestEntity
  final val Ir = InnerRecord

  import UnionTestEntity._

  test("filterPath") {
    assert(Rte.IdRef.filterPath == "_id")
    assert(Rte.ref(_.id).filterPath == "_id")
    assert(Rte.ref(_.int).filterPath == "int")
    assert(Rte.ref(_.renamedStr).filterPath == "str")
    assert(Rte.ref(_.intOpt).filterPath == "intOpt")
    assert(Rte.ref(_.intOpt.get).filterPath == "intOpt")
    assert(Rte.ref(_.intList.head).filterPath == "intList.0")
    assert(Rte.ref(_.intList(1)).filterPath == "intList.1")
    assert(Rte.ref(_.intMap("key")).filterPath == "intMap.key")
    assert(Rte.ref(_.intMap("weird.key")).filterPath == "intMap.weird\\_key")
    assert(Rte.ref(_.inner.int).filterPath == "inner.int")
    assert(Rte.ref(_.complex.get.apply(InnerId("foo")).apply(5).int).filterPath == "complex.foo.5.int")
    assert(Rte.ref(_.complex).ref(_.get).ref(_.apply(InnerId("foo"))).ref(_.apply(5)).ref(_.int).filterPath == "complex.foo.5.int")
    assert(Rte.ref(_.union.str).filterPath == "union.str")
    assert(Rte.ref(_.union.as[CaseOne]).filterPath == "union")
    assert(Rte.ref(_.union.as[CaseOne].str).filterPath == "union.str")
    assert(Rte.ref(_.union.as[CaseOne].data).filterPath == "union.data")
    assert(Rte.ref(_.union.as[HasInner].inner).filterPath == "union.inner")
    assert(Rte.ref(_.union).as[HasInner].ref(_.inner).filterPath == "union.inner")
    assert(Ute.ref(_.as[HasInner].inner).filterPath == "inner")
    assert(Ute.as[HasInner].ref(_.inner).filterPath == "inner")
    assert(Ute.ref(_.as[HasInner].inner.union.as[HasInner].inner).filterPath == "inner.union.inner")
  }

  test("projectionPath") {
    assert(Rte.ref(_.inner.int).projectionPath == "inner.int")
    assert(Rte.ref(_.intList).projectionPath == "intList")
    assert(Rte.ref(_.intList(1)).projectionPath == "intList")
    assert(Rte.ref(_.innerList.head.intList).projectionPath == "innerList")
    assert(Rte.ref(_.innerList.head.intList.head).projectionPath == "innerList")
  }

  def impliedFilterStr[E, T](ref: MongoRef[E, T]): String =
    MongoFilter.empty[E].toFilterBson(Opt.Empty, Set(ref)).toString

  test("implied filters") {
    assert(impliedFilterStr(Rte.SelfRef) ==
      "{}")
    assert(impliedFilterStr(Rte.ref(_.intOpt)) ==
      "{}")
    assert(impliedFilterStr(Rte.ref(_.intOpt.get)) ==
      """{"intOpt": {"$ne": null}}""")
    assert(impliedFilterStr(Ute.as[CaseOne]) ==
      """{"_case": {"$eq": "CaseOne"}}""")
    assert(impliedFilterStr(Ute.as[HasInner]) ==
      """{"_case": {"$in": ["CaseTwo", "CaseThree"]}}""")
    assert(impliedFilterStr(Ute.ref(_.as[HasInner].inner.innerOpt.get)) ==
      """{"inner.innerOpt": {"$ne": null}, "_case": {"$in": ["CaseTwo", "CaseThree"]}}""")
  }
}
