package com.avsystem.commons
package serialization

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class GenCodecUtilsTest extends AnyFunSuite with Matchers {
  import GenCodecUtilsTest.*

  test("plain case class") {
    val name = GenCodecUtils.codecTypeName[Foo]
    name shouldBe "Foo"
  }

  test("case class with name annotation") {
    val name = GenCodecUtils.codecTypeName[Bar]
    name shouldBe "OtherBar"
  }

  test("case class with name annotation - using GencodecTypeName typeclass") {
    val name = GencodecTypeName[Bar].name
    name shouldBe "OtherBar"
  }

  test("plain field") {
    val name = GenCodecUtils.codecFieldName[Bar](_.str)
    name shouldBe "str"
  }

  test("field with name annotation") {
    val name = GenCodecUtils.codecFieldName[Foo](_.str)
    name shouldBe "otherStr"
  }

  test("accessor chain disallowed") {
    "GenCodecUtils.codecFieldName[Complex](_.str.str)" shouldNot compile
  }

  test("subtypes count hierarchy") {
    GenCodecUtils.knownSubtypesCount[ExampleHierarchy] shouldBe 3
  }

  test("subtypes count leaf") {
    GenCodecUtils.knownSubtypesCount[CaseOther] shouldBe 0
  }
}

object GenCodecUtilsTest {
  final case class Foo(@name("otherStr") str: String)
  object Foo extends HasGenCodec[Foo]

  @name("OtherBar")
  final case class Bar(str: String)
  object Bar extends HasGenCodec[Bar]

  final case class Complex(str: Foo)
  object Complex extends HasGenCodec[Complex]

  sealed trait ExampleHierarchy
  case class Case123() extends ExampleHierarchy
  case object CaseObj987 extends ExampleHierarchy
  case class CaseOther() extends ExampleHierarchy
}
