package com.avsystem.commons
package serialization.json

import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.util.Random

/** [[EiselLemireDouble.parse]] must return exactly what `java.lang.Double.parseDouble` returns for every input (it is a
  * correctly-rounded fast path with a `parseDouble` fallback). Bit-equality is the oracle so NaN/-0.0 are covered.
  */
class EiselLemireDoubleTest extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {

  private def bits(d: Double): Long = java.lang.Double.doubleToLongBits(d)

  private def assertSame(s: String): Unit = {
    val expected =
      try java.lang.Double.parseDouble(s)
      catch { case _: NumberFormatException => return } // only compare on inputs parseDouble accepts
    val actual = EiselLemireDouble.parse(s)
    assert(bits(actual) == bits(expected), s"'$s' -> $actual (expected $expected)")
  }

  test("curated strings match Double.parseDouble") {
    val strings = List(
      "0",
      "0.0",
      "-0.0",
      "1",
      "-1",
      "1.0",
      "3.14",
      "3.141592653589793",
      "2.718281828459045",
      "0.1",
      "0.2",
      "0.3",
      "100",
      "-100",
      "12345.6789",
      "1e7",
      "1e-3",
      "1e10",
      "1e-10",
      "1e308",
      "1e-308",
      "1e309",
      "1e-320",
      "1e-324",
      "4.9e-324",
      "5e-324",
      "1.7976931348623157e308",
      "2.2250738585072014e-308",
      "9007199254740993",
      "1e23",
      "123456789012345678901234567890",
      "0.000000000000000000001",
      "9999999999999999999999",
      "1E7",
      "1.5E-5",
      "0.30000000000000004",
      "12.34e5",
      "-2.5e-10",
    )
    strings.foreach(assertSame)
  }

  test("infinity / nan / edge strings fall back correctly") {
    List("Infinity", "-Infinity", "NaN").foreach(assertSame)
  }

  test("integers of many lengths match") {
    (0 to 25).foreach { n =>
      assertSame("9" * math.max(1, n))
      assertSame("1" + "0" * n)
    }
  }

  test("property-based: Double.toString round-trips through parse") {
    forAll(arbitrary[Double]) { d =>
      if (java.lang.Double.isFinite(d)) {
        val s = d.toString
        assert(bits(EiselLemireDouble.parse(s)) == bits(d), s"'$s'")
      }
    }
  }

  test("property-based: arbitrary decimal strings match Double.parseDouble") {
    val gen = for {
      neg <- org.scalacheck.Gen.oneOf(true, false)
      intPart <- org.scalacheck.Gen.chooseNum(0L, Long.MaxValue)
      fracLen <- org.scalacheck.Gen.chooseNum(0, 25)
      frac <- org.scalacheck.Gen.listOfN(fracLen, org.scalacheck.Gen.numChar)
      exp <- org.scalacheck.Gen.chooseNum(-330, 330)
    } yield s"${if (neg) "-" else ""}$intPart.${frac.mkString}e$exp"
    forAll(gen)(assertSame)
  }

  test("massive sweep: random doubles' toString parses bit-identically") {
    val rnd = new Random(0xee15e1L)
    var i = 0
    var checked = 0
    while (i < 2000000) {
      val d = java.lang.Double.longBitsToDouble(rnd.nextLong())
      if (java.lang.Double.isFinite(d)) {
        val s = d.toString
        assert(bits(EiselLemireDouble.parse(s)) == bits(d), s"'$s'")
        checked += 1
      }
      i += 1
    }
    assert(checked > 1000000, s"expected many finite samples, got $checked")
  }

  test("massive sweep: random digit strings match Double.parseDouble") {
    val rnd = new Random(0xb0baL)
    var i = 0
    while (i < 500000) {
      val nDigits = 1 + rnd.nextInt(22)
      val sb = new StringBuilder
      if (rnd.nextBoolean()) sb.append('-')
      var d = 0
      while (d < nDigits) { sb.append(('0' + rnd.nextInt(10)).toChar); d += 1 }
      val dotPos = rnd.nextInt(nDigits + 1)
      sb.insert((if (sb.nonEmpty && sb.charAt(0) == '-') 1 else 0) + dotPos, '.')
      sb.append('e').append(rnd.nextInt(700) - 350)
      assertSame(sb.toString)
      i += 1
    }
  }
}
