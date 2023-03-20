package com.avsystem.commons
package misc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class NamedEnumTest extends AnyFunSuite with Matchers {

  sealed trait SomeNamedEnum extends NamedEnum

  trait WithName {
    val name = "I am from withName"
  }

  object TopLevel extends SomeNamedEnum {
    override val name: String = "I am toplvl"
  }

  sealed trait AnotherNamedEnum extends NamedEnum {
    override val name: String = "Another one"
  }

  case object Fourth extends AnotherNamedEnum with SomeNamedEnum

  object SomeNamedEnum extends NamedEnumCompanion[SomeNamedEnum] {

    case object First extends SomeNamedEnum {
      override val name: String = "First"
    }

    case object Second extends SomeNamedEnum {
      override lazy val name: String = "Second"
    }

    case object Third extends SomeNamedEnum with AnotherNamedEnum {
      override val name: String = "Third"
    }

    case object FromWithName extends SomeNamedEnum with WithName

    override val values: List[SomeNamedEnum] = caseObjects
  }

  object AnotherNamedEnum extends NamedEnumCompanion[AnotherNamedEnum] {
    override val values: List[AnotherNamedEnum] = caseObjects
  }

  test("all possible ways of `name` override") {
    import SomeNamedEnum._
    assert(byName == Map(
      "First" -> First,
      "Second" -> Second,
      "Third" -> Third,
      "Another one" -> Fourth,
      "I am toplvl" -> TopLevel,
      "I am from withName" -> FromWithName
    ))
  }

  test("object in two hierarchies") {
    SomeNamedEnum.byName.get("Another one") should contain(Fourth)
    AnotherNamedEnum.byName.get("Another one") should contain(Fourth)
  }

  test("object recognized by scope") {
    SomeNamedEnum.byName.get("Third") should contain(SomeNamedEnum.Third)
    AnotherNamedEnum.byName.get("Third") should contain(SomeNamedEnum.Third)
  }

  test("top level objects") {
    assert(SomeNamedEnum.byName.contains("I am toplvl"))
  }
}
