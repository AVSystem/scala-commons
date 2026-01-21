package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.mongo.text.TextSearchLanguage
import org.bson.BsonType
import org.scalatest.funsuite.AnyFunSuite

import java.util.regex.Pattern

class MongoFilterTest extends AnyFunSuite {
  final val Rte = RecordTestEntity
  final val Ute = UnionTestEntity
  final val Ir = InnerRecord

  final val IntRef: MongoPropertyRef[RecordTestEntity,Int] = Rte.ref(_.int)
  final val IntOptRef = Rte.ref(_.intOpt)
  final val IntsRef: MongoPropertyRef[RecordTestEntity,List[Int]] = Rte.ref(_.intList)
  final val StrRef: MongoPropertyRef[RecordTestEntity,String] = Rte.ref(_.renamedStr)

  test("empty") {
    assert(MongoFilter.empty[UnionTestEntity].toBson.toString == "{}")
  }

  test("simple operator filters") {
    assert(IntRef.hasType(BsonType.INT32).toBson.toString == """{"int": {"$type": 16}}""")
    assert(IntRef.exists.toBson.toString == """{"int": {"$exists": true}}""")
    assert(IntRef.exists(false).toBson.toString == """{"int": {"$exists": false}}""")

    assert(IntRef.is(0).toBson.toString == """{"int": {"$eq": 0}}""")
    assert(IntRef.isNot(0).toBson.toString == """{"int": {"$ne": 0}}""")

    assert(IntRef.in(Seq(1, 2, 3)).toBson.toString == """{"int": {"$in": [1, 2, 3]}}""")
    assert(IntRef.nin(Seq(1, 2, 3)).toBson.toString == """{"int": {"$nin": [1, 2, 3]}}""")

    assert(IntRef.in(1, 2, 3).toBson.toString == """{"int": {"$in": [1, 2, 3]}}""")
    assert(IntRef.nin(1, 2, 3).toBson.toString == """{"int": {"$nin": [1, 2, 3]}}""")

    assert(IntRef.gt(0).toBson.toString == """{"int": {"$gt": 0}}""")
    assert(IntRef.gte(0).toBson.toString == """{"int": {"$gte": 0}}""")
    assert(IntRef.lt(0).toBson.toString == """{"int": {"$lt": 0}}""")
    assert(IntRef.lte(0).toBson.toString == """{"int": {"$lte": 0}}""")

    assert((IntRef > 0).toBson.toString == """{"int": {"$gt": 0}}""")
    assert((IntRef >= 0).toBson.toString == """{"int": {"$gte": 0}}""")
    assert((IntRef < 0).toBson.toString == """{"int": {"$lt": 0}}""")
    assert((IntRef <= 0).toBson.toString == """{"int": {"$lte": 0}}""")

    assert(
      IntRef.satisfiesOperators(c => c.gte(0) ++ c.lte(10)).toBson.toString == """{"int": {"$gte": 0, "$lte": 10}}"""
    )

    assert(StrRef.regex("abc.*def").toBson.toString === """{"str": {"$regex": "abc.*def"}}""")
    assert(StrRef.regex("abc.*def", "i").toBson.toString === """{"str": {"$regex": "abc.*def", "$options": "i"}}""")
    assert(
      StrRef.regex(Pattern.compile("abc.*def", Pattern.CASE_INSENSITIVE | Pattern.MULTILINE)).toBson.toString ===
        """{"str": {"$regex": "abc.*def", "$options": "im"}}"""
    )
    assert(StrRef.startsWith("prefix").toBson.toString === """{"str": {"$regex": "^\\Qprefix\\E"}}""")
    assert(StrRef.containsSubstring("infix").toBson.toString === """{"str": {"$regex": "\\Qinfix\\E"}}""")
    assert(
      StrRef.containsSubstring("infix", caseInsensitive = true).toBson.toString ===
        """{"str": {"$regex": "\\Qinfix\\E", "$options": "i"}}"""
    )

    assert(
      StrRef.text("szekely", TextSearchLanguage.Hungarian, true, false).toBson.toString ==
        """{"str": {"$text": {"$search": "szekely", "$language": "hu", "$caseSensitive": true, "$diacriticSensitive": false}}}"""
    )

    assert(IntRef.mod(10, 0).toBson.toString == """{"int": {"$mod": [10, 0]}}""")
  }

  test("subtype filter") {
    assert(Ute.is[CaseOne].toBson.toString == """{"_case": {"$eq": "CaseOne"}}""")
    assert(Ute.is[HasInner].toBson.toString == """{"_case": {"$in": ["CaseTwo", "CaseThree"]}}""")
    assert(Ute.isNot[HasInner].toBson.toString == """{"_case": {"$nin": ["CaseTwo", "CaseThree"]}}""")
  }

  test("collection field filters") {
    assert(IntsRef.size(3).toBson.toString == """{"intList": {"$size": 3}}""")
    assert(IntsRef.isEmpty.toBson.toString == """{"intList": {"$size": 0}}""")
    assert(IntsRef.containsAny(Seq(1, 2, 3)).toBson.toString == """{"intList": {"$elemMatch": {"$in": [1, 2, 3]}}}""")
    assert(IntsRef.containsAny(1, 2, 3).toBson.toString == """{"intList": {"$elemMatch": {"$in": [1, 2, 3]}}}""")
    assert(IntsRef.contains(1).toBson.toString == """{"intList": {"$elemMatch": {"$eq": 1}}}""")
    assert(IntsRef.containsAll(Seq(1, 2, 3)).toBson.toString == """{"intList": {"$all": [1, 2, 3]}}""")
    assert(IntsRef.containsAll(1, 2, 3).toBson.toString == """{"intList": {"$all": [1, 2, 3]}}""")

    assert(IntsRef.elemMatch(_ > 5).toBson.toString == """{"intList": {"$elemMatch": {"$gt": 5}}}""")
    assert(
      IntsRef.elemMatch(_.satisfiesOperators(c => c.gte(0) ++ c.lte(10))).toBson.toString ==
        """{"intList": {"$elemMatch": {"$gte": 0, "$lte": 10}}}"""
    )
  }

  test("optional field filters") {
    assert(IntOptRef.isEmpty.toBson.toString == """{"intOpt": {"$eq": null}}""")
    assert(IntOptRef.isDefined.toBson.toString == """{"intOpt": {"$ne": null}}""")
  }

  test("with implied filters") {
    assert(
      Ute.as[CaseOne].ref(_.str).is("str").toBson.toString == """{"str": {"$eq": "str"}, "_case": {"$eq": "CaseOne"}}"""
    )
    assert(
      Rte.ref(_.intOpt.get).isNot(42).toBson.toString ==
        """{"$and": [{"intOpt": {"$ne": 42}}, {"intOpt": {"$ne": null}}]}"""
    )
  }

  test("logical operators") {
    assert(IntRef.not(_.is(5)).toBson.toString == """{"int": {"$not": {"$eq": 5}}}""")

    assert((IntRef.is(0) && IntRef.is(0)).toBson.toString == """{"int": {"$eq": 0}}""")
    assert((IntRef >= 0 && IntRef <= 10).toBson.toString == """{"int": {"$gte": 0, "$lte": 10}}""")
    assert((IntRef >= 0 && StrRef.is("foo")).toBson.toString == """{"int": {"$gte": 0}, "str": {"$eq": "foo"}}""")
    assert(
      (IntRef.in(1, 2, 3) && IntRef.in(2, 3, 4)).toBson.toString ==
        """{"$and": [{"int": {"$in": [1, 2, 3]}}, {"int": {"$in": [2, 3, 4]}}]}"""
    )

    assert((IntRef >= 0 || IntRef <= 10).toBson.toString == """{"$or": [{"int": {"$gte": 0}}, {"int": {"$lte": 10}}]}""")
  }

  test("filters on prefix property") {
    val Prefix = Rte.ref(_.inner)
    assert(Ir.ref(_.int).is(10).on(Prefix).toBson.toString == """{"inner.int": {"$eq": 10}}""")
    assert(Ir.ref(_.int).not(_.is(10)).on(Prefix).toBson.toString == """{"inner.int": {"$not": {"$eq": 10}}}""")
    assert(
      (Ir.ref(_.int).is(10) && Ir.ref(_.renamedStr).is("foo")).on(Prefix).toBson.toString ==
        """{"inner.int": {"$eq": 10}, "inner.str": {"$eq": "foo"}}"""
    )
    assert(
      (Ir.ref(_.int).is(10) || Ir.ref(_.renamedStr).is("foo")).on(Prefix).toBson.toString ==
        """{"$or": [{"inner.int": {"$eq": 10}}, {"inner.str": {"$eq": "foo"}}]}"""
    )
  }
}
