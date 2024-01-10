package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.mongo.typed.MongoUpdateOperator.CurrentDateType
import org.scalatest.funsuite.AnyFunSuite

class MongoUpdateTest extends AnyFunSuite {
  final val Rte = RecordTestEntity
  final val Ute = UnionTestEntity
  final val Ir = InnerRecord

  private def toString(update: MongoDocumentUpdate[_]): String = {
    val (bson, filters) = update.toBsonAndArrayFilters
    bson.toString + ", " + filters.toString
  }

  test("simple operators") {
    assert(toString(Rte.ref(_.int).set(5)) ==
      """{"$set": {"int": 5}}, []""")
    assert(toString(Rte.ref(_.int).setOnInsert(5)) ==
      """{"$setOnInsert": {"int": 5}}, []""")
    assert(toString(Rte.ref(_.int).inc(5)) ==
      """{"$inc": {"int": 5}}, []""")
    assert(toString(Rte.ref(_.int).min(5)) ==
      """{"$min": {"int": 5}}, []""")
    assert(toString(Rte.ref(_.int).max(5)) ==
      """{"$max": {"int": 5}}, []""")
    assert(toString(Rte.ref(_.int).mul(5)) ==
      """{"$mul": {"int": 5}}, []""")
    assert(toString(Rte.ref(_.tstamp).currentDate) ==
      """{"$currentDate": {"tstamp": true}}, []""")
    assert(toString(Rte.ref(_.tstamp).currentDate(CurrentDateType.Timestamp)) ==
      """{"$currentDate": {"tstamp": {"$type": "timestamp"}}}, []""")
    assert(toString(Rte.ref(_.intMap("fromKey")).rename(Rte.ref(_.intMap("toKey")))) ==
      """{"$rename": {"intMap.fromKey": "intMap.toKey"}}, []""")
    assert(toString(Rte.ref(_.int).unset) ==
      """{"$unset": {"int": ""}}, []""")
  }

  test("collection operators") {
    assert(toString(Rte.ref(_.intList).push(1, 2, 3)) ==
      """{"$push": {"intList": {"$each": [1, 2, 3]}}}, []""")
    assert(toString(Rte.ref(_.intList).push(sort = MongoOrder.ascending[Int])) ==
      """{"$push": {"intList": {"$each": [], "$sort": 1}}}, []""")
    assert(toString(Rte.ref(_.innerList).push(position = 0, slice = 5, sort = Ir.ref(_.int).ascending)) ==
      """{"$push": {"innerList": {"$each": [], "$position": 0, "$slice": 5, "$sort": {"int": 1}}}}, []""")
    assert(toString(Rte.ref(_.intList).addToSet(1, 2, 3)) ==
      """{"$addToSet": {"intList": {"$each": [1, 2, 3]}}}, []""")
    assert(toString(Rte.ref(_.intList).addToSet(Seq(1, 2, 3))) ==
      """{"$addToSet": {"intList": {"$each": [1, 2, 3]}}}, []""")
    assert(toString(Rte.ref(_.intList).popFirst) ==
      """{"$pop": {"intList": -1}}, []""")
    assert(toString(Rte.ref(_.intList).popLast) ==
      """{"$pop": {"intList": 1}}, []""")
    assert(toString(Rte.ref(_.intList).pull(_ < 5)) ==
      """{"$pull": {"intList": {"$lt": 5}}}, []""")
    assert(toString(Rte.ref(_.innerList).pull(_.ref(_.renamedStr).is("foo"))) ==
      """{"$pull": {"innerList": {"str": {"$eq": "foo"}}}}, []""")
    assert(toString(Rte.ref(_.intList).pullAll(1, 2, 3)) ==
      """{"$pullAll": {"intList": [1, 2, 3]}}, []""")
    assert(toString(Rte.ref(_.intList).pullAll(Seq(1, 2, 3))) ==
      """{"$pullAll": {"intList": [1, 2, 3]}}, []""")
    assert(toString(Rte.ref(_.intList).updateFirstMatching(_.set(5))) ==
      """{"$set": {"intList.$": 5}}, []""")
    assert(toString(Rte.ref(_.intList).updateAll(_.set(5))) ==
      """{"$set": {"intList.$[]": 5}}, []""")
    assert(toString(Rte.ref(_.intList).updateFiltered(_ < 0, _.set(5))) ==
      """{"$set": {"intList.$[filter0]": 5}}, [{"filter0": {"$lt": 0}}]""")
    assert(toString(Rte.ref(_.innerList).updateAll(_.ref(_.int).set(5))) ==
      """{"$set": {"innerList.$[].int": 5}}, []""")
    assert(toString(Rte.ref(_.innerList).updateFiltered(_.ref(_.int).lt(0), _.ref(_.int).set(5))) ==
      """{"$set": {"innerList.$[filter0].int": 5}}, [{"filter0.int": {"$lt": 0}}]""")
    assert(toString(Rte.ref(_.innerList).updateFiltered(_.ref(_.intMap("abc")).lt(0), _.ref(_.int).set(5))) ==
      """{"$set": {"innerList.$[filter0].int": 5}}, [{"filter0.intMap.abc": {"$lt": 0}}]""")
  }

  test("compound updates") {
    assert(toString(Rte.ref(_.int).inc(1) && Rte.ref(_.renamedStr).set("fooo")) ==
      """{"$inc": {"int": 1}, "$set": {"str": "fooo"}}, []""")
  }
}
