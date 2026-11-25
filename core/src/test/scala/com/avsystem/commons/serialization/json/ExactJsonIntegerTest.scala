package com.avsystem.commons
package serialization.json

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ExactJsonIntegerTest extends AnyFunSuite with ScalaCheckPropertyChecks {

  import ExactJsonInteger.parseLong

  private val accepted = List(
    "0" -> 0L,
    "-0" -> 0L,
    "+7" -> 7L,
    "007" -> 7L,
    "2.0" -> 2L,
    "5e3" -> 5000L,
    "5E+3" -> 5000L,
    "1000e-3" -> 1L,
    "1.5e1" -> 15L,
    "-0.0" -> 0L,
    "0e999999999" -> 0L,
    "0.000e-5" -> 0L,
    "-12.50e1" -> -125L,
    "9223372036854775807" -> Long.MaxValue,
    "9223372036854775807.000" -> Long.MaxValue,
    "9.223372036854775807e18" -> Long.MaxValue,
    "92233720368547758070e-1" -> Long.MaxValue,
    "-9223372036854775808" -> Long.MinValue,
    "-9223372036854775808.0" -> Long.MinValue,
    "-9.223372036854775808e18" -> Long.MinValue,
    "9007199254740993.0" -> 9007199254740993L,
    "922337203685477580e1" -> 9223372036854775800L,
    "-922337203685477580e1" -> -9223372036854775800L,
    "1e-0" -> 1L,
    "1E-00" -> 1L,
  )

  private val rejected = List(
    "2.0000000000000001",
    "9007199254740993.00000000000000000001",
    "2147483647.0000000001",
    "0.5",
    "-0.5",
    "1e-1",
    "9223372036854775808",
    "9223372036854775807.5",
    "-9223372036854775809",
    "9.223372036854775808e18",
    "1e19",
    "922337203685477581e1",
    "-922337203685477581e1",
    "1e999999999",
    "1e-999999999",
    "1e99999999999999999999",
    "",
    "-",
    "+",
    "1.",
    ".5",
    "1e",
    "1e+",
    "--1",
    "+-1",
    " 1",
    "1 ",
    "0x10",
    "1d",
    "1f",
    "Infinity",
    "NaN",
    "1_000",
    "١", // Arabic-Indic digit one
    "1.0.0",
    "1e1.5",
  )

  accepted.foreach { case (str, expected) =>
    test(s"accepts '$str' as $expected") {
      assert(parseLong(str) == expected)
    }
  }

  rejected.foreach { str =>
    test(s"rejects '$str'") {
      assertThrows[NumberFormatException](parseLong(str))
    }
  }

  test("huge exponents complete without expanding the number") {
    assertThrows[NumberFormatException](parseLong("1e999999999"))
    assertThrows[NumberFormatException](parseLong("1e-999999999"))
    assert(parseLong("0e999999999") == 0L)
  }

  // zeros are frequent so that whole numbers (trailing zeros, zero fractions) are common
  private val digit: Gen[Char] = Gen.frequency(3 -> Gen.const('0'), 2 -> Gen.numChar)
  private def digits(minCount: Int, maxCount: Int): Gen[String] =
    Gen.choose(minCount, maxCount).flatMap(n => Gen.listOfN(n, digit).map(_.mkString))

  private val numberText: Gen[String] = for {
    sign <- Gen.oneOf("", "-", "+")
    // short integer parts are common so that in-range values are common
    intPart <- Gen.frequency(6 -> digits(1, 6), 3 -> digits(7, 19), 1 -> digits(20, 25))
    // an all-zero fraction keeps the number whole
    fraction <- Gen.option(Gen.oneOf(digits(1, 25), Gen.choose(1, 25).map("0" * _)).map("." + _))
    exponent <- Gen.option(for {
      e <- Gen.oneOf("e", "E")
      expValue <- Gen.choose(-40, 40)
      expSign <- if (expValue < 0) Gen.const("-") else Gen.oneOf("", "+")
    } yield s"$e$expSign${expValue.abs}")
  } yield s"$sign$intPart${fraction.getOrElse("")}${exponent.getOrElse("")}"

  test("agrees with exact BigDecimal conversion") {
    forAll(numberText, minSuccessful(2000)) { str =>
      val exact = new java.math.BigDecimal(str)
      // on Scala.js, longValueExact wrongly throws for zero with a large negative scale, e.g. 0E34
      val expected = if (exact.signum == 0) Success(0L) else Try(exact.longValueExact())
      expected match {
        case Success(value) => assert(parseLong(str) == value, str)
        case Failure(_) => assertThrows[NumberFormatException](parseLong(str))
      }
    }
  }
}
