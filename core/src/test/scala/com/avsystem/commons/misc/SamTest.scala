package com.avsystem.commons
package misc

import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.nowarn

class SamTest extends AnyFunSuite {

  test("no arg lists by name") {
    trait NoArgListsSam {
      def handle: String
    }
    object NoArgListsSam extends SamCompanion[NoArgListsSam, String]

    val sam = NoArgListsSam("42")
    assert(sam.handle == "42")

    val adhoc = Sam[NoArgListsSam]("42")
    assert(adhoc.handle == "42")
  }

  test("no args") {
    trait NoArgSam {
      def handle(): String
    }
    object NoArgSam extends SamCompanion[NoArgSam, () => String]

    val sam = NoArgSam(() => "42")
    assert(sam.handle() == "42")

    val adhoc = Sam[NoArgSam](() => "42")
    assert(adhoc.handle() == "42")
  }

  test("no args by name") {
    trait NoArgSam {
      def handle(): String
    }
    object NoArgSam extends SamCompanion[NoArgSam, String]

    @nowarn
    val sam = NoArgSam("42")
    assert(sam.handle() == "42")

    @nowarn
    val adhoc = Sam[NoArgSam]("42")
    assert(adhoc.handle() == "42")
  }

  test("single arg") {
    trait SingleArgSam {
      def handle(i: Int): String
    }
    object SingleArgSam extends SamCompanion[SingleArgSam, Int => String]

    val sam = SingleArgSam(_.toString)
    assert(sam.handle(123) == "123")

    val adhoc = Sam[SingleArgSam]((_: Int).toString)
    assert(adhoc.handle(123) == "123")
  }

  test("two args") {
    trait TwoArgSam {
      def handle(i: Int, str: String): String
    }
    object TwoArgSam extends SamCompanion[TwoArgSam, (Int, String) => String]

    val sam = TwoArgSam(_.toString + _)
    assert(sam.handle(123, "lol") == "123lol")

    val adhoc = Sam[TwoArgSam]((i: Int, s: String) => i.toString + s)
    assert(adhoc.handle(123, "lol") == "123lol")
  }

  test("multiple arg lists") {
    trait MultipleArgListsSam {
      def handle(i: Int)(str: String): String
    }
    object MultipleArgListsSam extends SamCompanion[MultipleArgListsSam, Int => String => String]

    val fullSam = MultipleArgListsSam(i => s => i.toString + s)
    assert(fullSam.handle(123)("lol") == "123lol")

    def ptApplied(i: Int) = (s: String) => i.toString + s
    val partialSam = MultipleArgListsSam(i => ptApplied(i))
    assert(partialSam.handle(123)("lol") == "123lol")

    val adhoc = Sam[MultipleArgListsSam]((i: Int) => (s: String) => i.toString + s)
    assert(adhoc.handle(123)("lol") == "123lol")
  }

  test("implicit params") {
    trait ImplicitParamsSam {
      def handle(i: Int)(implicit str: String): String
    }
    object ImplicitParamsSam extends SamCompanion[ImplicitParamsSam, Int => String => String]

    val sam = ImplicitParamsSam(i => s => i.toString + s)
    assert(sam.handle(123)(using "lol") == "123lol")

    val adhoc = Sam[ImplicitParamsSam]((i: Int) => (s: String) => i.toString + s)
    assert(adhoc.handle(123)(using "lol") == "123lol")
  }
}
