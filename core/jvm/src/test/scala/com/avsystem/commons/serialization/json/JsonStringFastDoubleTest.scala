package com.avsystem.commons
package serialization.json

import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.util.Random

/** Verifies the [[JsonNumberCodec.Fast]] double formatter in [[JsonStringOutput]] (backed by [[XjbDouble]]). Fast mode
  * is not required to be character-identical to `Double.toString`, so the correctness oracle is round-trip fidelity:
  * the produced JSON number must parse back to the exact same `Double`.
  */
class JsonStringFastDoubleTest extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {

  private val fast = JsonOptions(numberCodec = JsonNumberCodec.Fast)

  private def bits(d: Double): Long = java.lang.Double.doubleToLongBits(d)

  private def assertRoundTrips(d: Double): Unit = {
    val text = JsonStringOutput.write(d, fast)
    // Must be valid JSON that the library's own reader accepts, round-tripping to the exact same double.
    val viaReader = JsonStringInput.read[Double](text)
    assert(bits(viaReader) == bits(d), s"library round-trip: $d -> '$text' -> $viaReader")
    // And parse identically via the JDK.
    val viaJdk = java.lang.Double.parseDouble(text)
    assert(bits(viaJdk) == bits(d), s"JDK round-trip: $d -> '$text' -> $viaJdk")
  }

  test("special and boundary values round-trip") {
    val specials = List(
      0.0,
      -0.0,
      1.0,
      -1.0,
      2.0,
      0.5,
      0.1,
      0.2,
      0.3,
      100.0,
      -100.0,
      12345.6789,
      1e7,
      1e-3,
      9.999999e6,
      1e-4,
      123456789.0,
      Double.MinPositiveValue,
      java.lang.Double.MIN_VALUE,
      java.lang.Double.MAX_VALUE,
      -java.lang.Double.MAX_VALUE,
      java.lang.Double.MIN_NORMAL,
      1.7976931348623157e308,
      2.2250738585072014e-308,
      4.9e-324,
      1.0e23,
      9.007199254740993e15,
      5.0e-324,
      1.0e-323,
    )
    specials.foreach(assertRoundTrips)
  }

  test("powers of ten round-trip") {
    (-323 to 308).foreach { e =>
      val d = java.lang.Double.parseDouble(s"1e$e")
      if (java.lang.Double.isFinite(d) && d != 0.0) assertRoundTrips(d)
    }
  }

  test("non-finite values are quoted, identical to Standard") {
    List(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity).foreach { d =>
      val fastStr = JsonStringOutput.write(d, fast)
      val stdStr = JsonStringOutput.write(d, JsonOptions.Default)
      assert(fastStr == stdStr, s"non-finite $d")
      assert(fastStr.startsWith("\"") && fastStr.endsWith("\""))
    }
  }

  test("property-based: arbitrary doubles round-trip") {
    forAll(arbitrary[Double]) { d =>
      if (java.lang.Double.isFinite(d)) assertRoundTrips(d)
    }
  }

  test("massive random bit-pattern sweep round-trips") {
    val rnd = new Random(0xc0ffeeL)
    var i = 0
    var checked = 0
    while (i < 2000000) {
      val d = java.lang.Double.longBitsToDouble(rnd.nextLong())
      if (java.lang.Double.isFinite(d)) {
        assertRoundTrips(d)
        checked += 1
      }
      i += 1
    }
    assert(checked > 1000000, s"expected many finite samples, got $checked")
  }

  test("Map[String, Double] serializes and round-trips in Fast mode") {
    val m = (0 until 200).map(i => s"f$i" -> (i + 0.123456789) * math.pow(10, (i % 9) - 4)).toMap
    val json = JsonStringOutput.write(m, fast)
    val parsed = JsonStringInput.read[Map[String, Double]](json)
    parsed.foreach { case (k, v) => assert(bits(v) == bits(m(k)), s"key $k") }
    assert(parsed == m)
  }
}
