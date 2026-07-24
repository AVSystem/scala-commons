package com.avsystem.commons
package serialization.json

import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.util.Random

/** Verifies `Float` support in [[JsonStringOutput]] (XjbFloat) and [[JsonStringInput]] (EiselLemireFloat). Writing must
  * round-trip; reading must be bit-identical to `java.lang.Float.parseFloat`.
  */
class JsonStringFastFloatTest extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {

  private def bits(f: Float): Int = java.lang.Float.floatToIntBits(f)
  private def writeF(f: Float): String = JsonStringOutput.write(f)

  private def assertWriteRoundTrips(f: Float): Unit = {
    val text = writeF(f)
    assert(bits(java.lang.Float.parseFloat(text)) == bits(f), s"JDK round-trip: $f -> '$text'")
    assert(bits(JsonStringInput.read[Float](text)) == bits(f), s"library round-trip: $f -> '$text'")
  }

  private def assertReadSame(s: String): Unit = {
    val expected =
      try java.lang.Float.parseFloat(s)
      catch { case _: NumberFormatException => return }
    assert(
      bits(EiselLemireFloat.parse(s)) == bits(expected),
      s"EiselLemireFloat '$s' -> ${EiselLemireFloat.parse(s)} (expected $expected)",
    )
  }

  private val specials = List(
    0.0f,
    -0.0f,
    1.0f,
    -1.0f,
    0.1f,
    0.2f,
    0.3f,
    3.14f,
    100.0f,
    -100.0f,
    12345.678f,
    1e7f,
    1e-3f,
    1e-4f,
    Float.MinPositiveValue,
    java.lang.Float.MIN_VALUE,
    java.lang.Float.MAX_VALUE,
    -java.lang.Float.MAX_VALUE,
    java.lang.Float.MIN_NORMAL,
    3.4028235e38f,
    1.4e-45f,
    0.33007812f,
    16777216.0f,
  )

  test("special/boundary floats write-round-trip") {
    specials.foreach(assertWriteRoundTrips)
  }

  test("write matches Float.toString (ignoring exponent case) for common values") {
    List(0.0f, -0.0f, 1.0f, -1.0f, 0.5f, 3.14f, 100.0f, 12345.678f, 1e7f, 1e-3f, 0.1f).foreach { f =>
      assert(writeF(f).equalsIgnoreCase(f.toString), s"$f: ryu=${writeF(f)} jdk=${f.toString}")
    }
  }

  test("read matches Float.parseFloat for curated strings") {
    List(
      "0",
      "0.0",
      "-0.0",
      "1",
      "3.14",
      "0.1",
      "100",
      "12345.678",
      "1e7",
      "1e-3",
      "1.4e-45",
      "3.4028235e38",
      "16777216",
      "16777217",
      "1.23456789",
      "0.000001",
      "9999999",
      "1e20",
      "1e-30",
    ).foreach(assertReadSame)
  }

  test("non-finite floats: write quoted, read back") {
    List(Float.NaN, Float.PositiveInfinity, Float.NegativeInfinity).foreach { f =>
      val text = writeF(f)
      assert(text.startsWith("\"") && text.endsWith("\""))
      assert(bits(JsonStringInput.read[Float](text)) == bits(f))
    }
  }

  test("floats embedded in an array read via the buffer path") {
    val json = specials.map(_.toString).mkString("[", ",", "]")
    val read = JsonStringInput.read[List[Float]](json)
    read.zip(specials).foreach { case (r, v) => assert(bits(r) == bits(v), s"$v -> $r") }
  }

  test("property-based: arbitrary floats write-round-trip and read identically") {
    forAll(arbitrary[Float]) { f =>
      if (java.lang.Float.isFinite(f)) {
        assertWriteRoundTrips(f)
        assertReadSame(f.toString)
      }
    }
  }

  test("massive sweep: random float bit patterns write-round-trip and read bit-exactly") {
    val rnd = new Random(0xf10a7L)
    var i = 0
    var checked = 0
    while (i < 2000000) {
      val f = java.lang.Float.intBitsToFloat(rnd.nextInt())
      if (java.lang.Float.isFinite(f)) {
        assertWriteRoundTrips(f)
        assert(bits(EiselLemireFloat.parse(f.toString)) == bits(f), s"read ${f.toString}")
        checked += 1
      }
      i += 1
    }
    assert(checked > 1000000, s"expected many finite samples, got $checked")
  }
}
