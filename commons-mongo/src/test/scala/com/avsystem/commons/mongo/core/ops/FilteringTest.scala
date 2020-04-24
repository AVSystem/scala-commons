package com.avsystem.commons
package mongo.core.ops

import java.util.regex.Pattern

import com.avsystem.commons.mongo.BsonRef
import com.avsystem.commons.serialization.GenCodec
import com.mongodb.client.model.Filters
import org.bson.BsonType
import org.bson.conversions.Bson
import org.scalatest.funsuite.AnyFunSuite

class FilteringTest extends AnyFunSuite {

  import Filtering._
  import FilteringTest._

  private def testCase(name: String)(filter: (Ref[String]) => Bson)(verify: (String) => Bson): Unit = {
    import BsonEquality.bsonEquality

    test(name) {
      assert(filter(sRef) === verify("s"))
    }
  }

  private def testValue(name: String)(filter: (Ref[String], String) => Bson)(verify: (String, String) => Bson): Unit = {
    val someValue = "someValue"
    testCase(name)(filter(_, someValue))(verify(_, someValue))
  }

  testValue("equal")(_ equal _)(Filters.eq)
  testValue("notEqual")(_ notEqual _)(Filters.ne)

  testValue("gt")(_ gt _)(Filters.gt)
  testValue("lt")(_ lt _)(Filters.lt)
  testValue("gte")(_ gte _)(Filters.gte)
  testValue("lte")(_ lte _)(Filters.lte)

  testValue("in")(_ in _)(Filters.in(_, _))
  testValue("nin")(_ nin _)(Filters.nin(_, _))

  testCase("exists")(_.exists())(Filters.exists)
  testCase("notExists")(_.exists(false))(Filters.exists(_, false))

  testCase("ofType")(_.ofType("someTypeName"))(Filters.`type`(_, "someTypeName"))
  testCase("ofTypeEnum")(_.ofType(BsonType.STRING))(Filters.`type`(_, BsonType.STRING))

  testCase("mod")(_.mod(313, 131))(Filters.mod(_, 313, 131))

  private val regexString = "\\d"
  private val regexScala = regexString.r
  private val regexJava = Pattern.compile(regexString)
  testCase("regexScala")(_ regex regexScala)(Filters.regex(_, regexString))
  testCase("regexJava")(_ regex regexJava)(Filters.regex(_, regexJava))
  testCase("regexString")(_ regex regexString)(Filters.regex(_, regexString))
  testCase("regexOptions")(_.regex(regexString, "ops"))(Filters.regex(_, regexString, "ops"))

  import BsonEquality.bsonEquality

  test("contains") {
    assert(aRef.contains("elem") === Filters.eq("a", "elem"))
  }

  private val simpleFilter = Filters.eq("key", "value")
  test("elemMatch") {
    assert(aRef.elemMatch(simpleFilter) === Filters.elemMatch("a", simpleFilter))
  }

  test("size") {
    assert(aRef.size(131) === Filters.size("a", 131))
  }

  test("all") {
    assert(aRef.all("e1", "e2") === Filters.all("a", "e1", "e2"))
  }

  private val otherFilter = Filters.eq("key2", "value2")
  test("and") {
    assert(and(simpleFilter, otherFilter) === Filters.and(simpleFilter, otherFilter))
  }

  test("or") {
    assert(or(simpleFilter, otherFilter) === Filters.or(simpleFilter, otherFilter))
  }

  test("nor") {
    assert(nor(simpleFilter, otherFilter) === Filters.nor(simpleFilter, otherFilter))
  }

  test("not") {
    assert(not(simpleFilter) === Filters.not(simpleFilter))
  }
}

object FilteringTest extends BsonRef.Creator[SomeEntity] {
  implicit val codec: GenCodec[SomeEntity] = GenCodec.materialize
  val sRef: Ref[String] = ref(_.s)
  val aRef: Ref[List[String]] = ref(_.a)
}
