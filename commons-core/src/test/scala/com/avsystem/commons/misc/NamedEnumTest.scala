package com.avsystem.commons
package misc


import org.scalatest.FunSuite

class NamedEnumTest extends FunSuite {

  sealed trait SomeNamedEnum extends NamedEnum {
    override def name: String = "This is enum"
  }

  object TopLevel extends SomeNamedEnum {
    override def name: String = "I am toplvl"
  }

  sealed trait AnotherNamedEnum extends NamedEnum {
    override def name: String = "Another one"
  }

  object SomeNamedEnum extends NamedEnumCompanion[SomeNamedEnum] {

    case object First extends SomeNamedEnum {
      override def name: String = "First"
    }

    case object Second extends SomeNamedEnum {
      override val name: String = "Second"
    }

    case object Third extends SomeNamedEnum {
      override lazy val name: String = "Third"
    }

    case object Fifth extends SomeNamedEnum with AnotherNamedEnum

    case object Sixth extends AnotherNamedEnum with SomeNamedEnum

    case class Seventh() extends SomeNamedEnum {
      override def name: String = "I am a case class"
    }

    override val values: List[SomeNamedEnum] = caseObjects
    val names: Map[String, SomeNamedEnum] = byName
  }

  import SomeNamedEnum._

  test("named case objects listing test with all possible ways of `name` override") {
    assert(names == Map(
      "First" -> First,
      "Second" -> Second,
      "Third" -> Third,
      "This is enum" -> Sixth,
      "Another one" -> Fifth,
      "I am toplvl" -> TopLevel
    ))
  }

  test("One should be careful with scala linearization") {
    assert(names.get("Another one").get == Fifth)
    assert(names.get("This is enum").get == Sixth)
  }

  test("Top level objects works fine") {
    assert(names.contains("I am toplvl"))
  }

  test("This one is missing") {
    case object Missing extends SomeNamedEnum {
      override def name: String = "missing"
    }
    assert(!names.keySet.contains("missing"))
  }

  test("One can call byName here") {
    assert(SomeNamedEnum.byName == Map(
      "First" -> First,
      "Second" -> Second,
      "Third" -> Third,
      "This is enum" -> Sixth,
      "Another one" -> Fifth,
      "I am toplvl" -> TopLevel
    ))
  }

  test("Case class is not here") {
    assert(!names.contains("I am a case class"))
  }
}
