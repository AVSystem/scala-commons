package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.mongo.typed.MongoUpdateOperator.CurrentDateType
import org.scalatest.funsuite.AnyFunSuite

class MongoUpdateTest extends AnyFunSuite {
  final val Rte = RecordTestEntity
  final val Ute = UnionTestEntity
  final val Ir = InnerRecord

  test("simple operators") {
    assert(Rte.ref(_.int).set(5).toBson.toString ==
      """{"$set": {"int": 5}}""")
    assert(Rte.ref(_.int).setOnInsert(5).toBson.toString ==
      """{"$setOnInsert": {"int": 5}}""")
    assert(Rte.ref(_.int).inc(5).toBson.toString ==
      """{"$inc": {"int": 5}}""")
    assert(Rte.ref(_.int).min(5).toBson.toString ==
      """{"$min": {"int": 5}}""")
    assert(Rte.ref(_.int).max(5).toBson.toString ==
      """{"$max": {"int": 5}}""")
    assert(Rte.ref(_.int).mul(5).toBson.toString ==
      """{"$mul": {"int": 5}}""")
    assert(Rte.ref(_.tstamp).currentDate.toBson.toString ==
      """{"$currentDate": {"tstamp": true}}""")
    assert(Rte.ref(_.tstamp).currentDate(CurrentDateType.Timestamp).toBson.toString ==
      """{"$currentDate": {"tstamp": {"$type": "timestamp"}}}""")
    assert(Rte.ref(_.intMap("fromKey")).rename(Rte.ref(_.intMap("toKey"))).toBson.toString ==
      """{"$rename": {"intMap.fromKey": "intMap.toKey"}}""")
    assert(Rte.ref(_.int).unset.toBson.toString ==
      """{"$unset": {"int": ""}}""")
  }

  test("collection operators") {
    assert(Rte.ref(_.intList).push(1, 2, 3).toBson.toString ==
      """{"$push": {"intList": {"$each": [1, 2, 3]}}}""")
    assert(Rte.ref(_.intList).push(sort = MongoOrder.ascending[Int]).toBson.toString ==
      """{"$push": {"intList": {"$each": [], "$sort": 1}}}""")
    assert(Rte.ref(_.innerList).push(position = 0, slice = 5, sort = Ir.ref(_.int).ascending).toBson.toString ==
      """{"$push": {"innerList": {"$each": [], "$position": 0, "$slice": 5, "$sort": {"int": 1}}}}""")
    assert(Rte.ref(_.intList).addToSet(1, 2, 3).toBson.toString ==
      """{"$addToSet": {"intList": {"$each": [1, 2, 3]}}}""")
    assert(Rte.ref(_.intList).addToSet(Seq(1, 2, 3)).toBson.toString ==
      """{"$addToSet": {"intList": {"$each": [1, 2, 3]}}}""")
    assert(Rte.ref(_.intList).popFirst.toBson.toString ==
      """{"$pop": {"intList": -1}}""")
    assert(Rte.ref(_.intList).popLast.toBson.toString ==
      """{"$pop": {"intList": 1}}""")
    assert(Rte.ref(_.intList).pull(_ < 5).toBson.toString ==
      """{"$pull": {"intList": {"$lt": 5}}}""")
    assert(Rte.ref(_.innerList).pull(_.ref(_.renamedStr).is("foo")).toBson.toString ==
      """{"$pull": {"innerList": {"str": {"$eq": "foo"}}}}""")
    assert(Rte.ref(_.intList).pullAll(1, 2, 3).toBson.toString ==
      """{"$pullAll": {"intList": [1, 2, 3]}}""")
    assert(Rte.ref(_.intList).pullAll(Seq(1, 2, 3)).toBson.toString ==
      """{"$pullAll": {"intList": [1, 2, 3]}}""")
  }

  test("compound updates") {
    assert((Rte.ref(_.int).inc(1) && Rte.ref(_.renamedStr).set("fooo")).toBson.toString ==
      """{"$inc": {"int": 1}, "$set": {"str": "fooo"}}""")
  }
}
