package com.avsystem.commons
package mongo

import com.mongodb.internal.validator.CollectibleDocumentFieldNameValidator
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

class KeyEscaperTest extends FunSuite with PropertyChecks {

  import KeyEscaper._
  import KeyEscaperTest._

  test("custom keys") {
    val customCases = List(
      "plain" -> "plain",
      "<plain, but strange>" -> "<plain, but strange>",
      "not_so_plain" -> "not__so__plain",
      "$" -> "_D",
      "." -> "_d"
    )

    for ((input, expected) <- customCases) {
      val escaped = escape(input)
      assert(validator.validate(escaped))
      assert(escaped === expected)
      assert(escaped.forall(isPlain))
      assert(unescape(escaped) === input)
    }
  }

  test("plain keys") {
    forAll(plainKeyGen) { plainKey =>
      val escaped = escape(plainKey)
      assert(validator.validate(escaped))
      assert(escaped === plainKey)
      assert(escaped.forall(isPlain))
      assert(unescape(escaped) === plainKey)
    }
  }

  test("arbitrary keys") {
    forAll(deniedKeyGen) { arbitraryKey =>
      val escaped = escape(arbitraryKey)
      assert(validator.validate(escaped))
      assert(escaped.forall(isPlain))
      assert(unescape(escaped) === arbitraryKey)
    }
  }
}

object KeyEscaperTest {
  def isPlain(char: Char): Boolean = char != '.' && char != '$'

  val validator = new CollectibleDocumentFieldNameValidator

  val plainCharGen: Gen[Char] = Arbitrary.arbitrary[Char].filter(isPlain)
  val plainKeyGen: Gen[String] = Gen.listOf(plainCharGen).map(_.mkString)

  val deniedCharGen: Gen[Char] = Gen.oneOf('.', '$')
  val deniedKeyGen: Gen[String] = Gen.listOf(Gen.oneOf(plainCharGen, deniedCharGen)).map(_.mkString)
}
