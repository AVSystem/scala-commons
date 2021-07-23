package com.avsystem.commons
package mongo

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class KeyEscaperTest extends AnyFunSuite with ScalaCheckPropertyChecks {

  import KeyEscaper._
  import KeyEscaperTest._

  test("custom keys") {
    val customCases = List(
      "plain" -> "plain",
      "<plain, but strange>" -> "<plain, but strange>",
      "not_so_plain" -> "not_so_plain",
      "$" -> "\\$",
      "." -> "\\_",
      "plain$ with$ $dollars$" -> "plain$ with$ $dollars$",
      "Sentence." -> "Sentence\\_",
      "$operator" -> "\\$operator",
      "$worst.of.both.worlds" -> "\\$worst\\_of\\_both\\_worlds"
    )

    for ((input, expected) <- customCases) {
      val escaped = escape(input)
      assert(escaped == expected)
      assert(unescape(escaped) == input)
    }
  }

  test("plain keys") {
    forAll(plainKeyGen) { plainKey =>
      val escaped = escape(plainKey)
      assert(escaped == plainKey)
      assert(unescape(escaped) == plainKey)
    }
  }

  test("arbitrary keys") {
    forAll(deniedKeyGen) { arbitraryKey =>
      val escaped = escape(arbitraryKey)
      assert(unescape(escaped) == arbitraryKey)
    }
  }
}

object KeyEscaperTest {
  def isPlain(char: Char): Boolean = char != '.' && char != '$' && char != '\\'

  val plainCharGen: Gen[Char] = Arbitrary.arbitrary[Char].filter(isPlain)
  val plainKeyGen: Gen[String] = Gen.listOf(plainCharGen).map(_.mkString)

  val deniedCharGen: Gen[Char] = Gen.oneOf('.', '$')
  val deniedKeyGen: Gen[String] = Gen.listOf(Gen.oneOf(plainCharGen, deniedCharGen)).map(_.mkString)
}
