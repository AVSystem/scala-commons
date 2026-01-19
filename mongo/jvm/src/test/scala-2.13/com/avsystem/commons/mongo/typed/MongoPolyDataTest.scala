package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.serialization.flatten
import org.scalatest.funsuite.AnyFunSuite

@flatten sealed trait PolyMongoUnion[+T]
object PolyMongoUnion extends MongoPolyDataCompanion[PolyMongoUnion] {
  case class CaseOne[+T](value: T, str: String) extends PolyMongoUnion[T]
  case class CaseTwo[+T](foo: T, int: Int) extends PolyMongoUnion[T]
  case object CaseThree extends PolyMongoUnion[Nothing]
}

case class PolyMongoRecord[+T](value: T, meta: String)
object PolyMongoRecord extends MongoPolyDataCompanion[PolyMongoRecord]

class MongoPolyDataTest extends AnyFunSuite {
  final val Pmr = PolyMongoRecord
  final val Pmu = PolyMongoUnion

  test("filterPath") {
    assert(Pmr.dsl[Int].ref(_.value).rawPath == "value")
    assert(Pmu.dsl[Int].ref(_.as[PolyMongoUnion.CaseOne[Int]].value).rawPath == "value")
    assert(Pmu.dsl[Int].as[PolyMongoUnion.CaseOne[Int]].ref(_.value).rawPath == "value")
    assert(Pmu.dsl[Int].ref(_.as[PolyMongoUnion.CaseOne[Int]].str).rawPath == "str")
    assert(
      Pmu
        .dsl[PolyMongoUnion[Int]]
        .ref(x => x.as[PolyMongoUnion.CaseOne[PolyMongoUnion[Int]]].value.as[PolyMongoUnion.CaseOne[Int]].str)
        .rawPath == "value.str"
    )
  }
}
