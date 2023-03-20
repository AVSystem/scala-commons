package com.avsystem.commons
package mongo.typed

import org.scalatest.funsuite.AnyFunSuite

class MongoRefTest extends AnyFunSuite {
  final val Rte = RecordTestEntity
  final val Ute = UnionTestEntity
  final val Ir = InnerRecord

  test("filterPath") {
    assert(Rte.IdRef.rawPath == "_id")
    assert(Rte.ref(_.id).rawPath == "_id")
    assert(Rte.ref(_.int).rawPath == "int")
    assert(Rte.ref(_.renamedStr).rawPath == "str")
    assert(Rte.ref(_.intOpt).rawPath == "intOpt")
    assert(Rte.ref(_.intOpt.get).rawPath == "intOpt")
    assert(Rte.ref(_.intList.head).rawPath == "intList.0")
    assert(Rte.ref(_.intList(1)).rawPath == "intList.1")
    assert(Rte.ref(_.intMap("key")).rawPath == "intMap.key")
    assert(Rte.ref(_.intMap("weird.key")).rawPath == "intMap.weird\\_key")
    assert(Rte.ref(_.typedMap(PKey.IntKey)).rawPath == "typedMap.IntKey")
    assert(Rte.ref(_.typedMap(PKey.InnerKey).int).rawPath == "typedMap.InnerKey.int")
    assert(Rte.ref(_.inner.int).rawPath == "inner.int")
    assert(Rte.ref(_.complex.get.apply(InnerId("foo")).apply(5).int).rawPath == "complex.foo.5.int")
    assert(Rte.ref(_.complex).ref(_.get).ref(_.apply(InnerId("foo"))).ref(_.apply(5)).ref(_.int).rawPath == "complex.foo.5.int")
    assert(Rte.ref(_.props.map("key")).rawPath == "props.key")
    assert(Rte.ref(_.union.str).rawPath == "union.str")
    assert(Rte.ref(_.union.as[CaseOne]).rawPath == "union")
    assert(Rte.ref(_.union.as[CaseOne].str).rawPath == "union.str")
    assert(Rte.ref(_.union.as[CaseOne].data).rawPath == "union.data")
    assert(Rte.ref(_.union.as[HasInner].inner).rawPath == "union.inner")
    assert(Rte.ref(_.union).as[HasInner].ref(_.inner).rawPath == "union.inner")
    assert(Ute.ref(_.as[HasInner].inner).rawPath == "inner")
    assert(Ute.as[HasInner].ref(_.inner).rawPath == "inner")
    assert(Ute.ref(_.as[HasInner].inner.union.as[HasInner].inner).rawPath == "inner.union.inner")
    assert((Ute.ref(_.as[HasInner].inner) andThen Rte.ref(_.union.as[HasInner].inner)).rawPath == "inner.union.inner")
    assert((Rte.ref(_.union.as[HasInner].inner) compose Ute.ref(_.as[HasInner].inner)).rawPath == "inner.union.inner")
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
