package com.avsystem.commons
package serialization.json

import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.util.Random

/** Exercises [[JsonStringInput.readDouble]] end-to-end (numbers embedded in a larger JSON document, read through the
  * full deserialization path). The value read back must be bit-identical to the original.
  */
class JsonReadDoubleTest extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {

  private def bits(d: Double): Long = java.lang.Double.doubleToLongBits(d)

  // readDouble parses the number's text with the Eisel-Lemire parser (the path under test).
  private def readList(json: String): List[Double] = JsonStringInput.read[List[Double]](json)

  test("doubles embedded in a JSON array round-trip bit-exactly") {
    val values = List(0.0, -0.0, 1.0, -1.0, 3.14, 0.1, 1e7, 1e-3, 1.7976931348623157e308, 2.2250738585072014e-308,
      4.9e-324, 1.0e23, 123456789.0, -2.5e-10, 9.007199254740993e15)
    val json = values.map(_.toString).mkString("[", ",", "]")
    val read = readList(json)
    read.zip(values).foreach { case (r, v) => assert(bits(r) == bits(v), s"$v -> $r in $json") }
  }

  test("doubles as object field values round-trip") {
    val m = Map("a" -> 3.14, "b" -> -1e-9, "c" -> 1e20, "d" -> 0.0)
    val json = JsonStringOutput.write(m)
    val read = JsonStringInput.read[Map[String, Double]](json)
    m.foreach { case (k, v) => assert(bits(read(k)) == bits(v), s"key $k") }
  }

  test("non-finite doubles (quoted) still read correctly") {
    val values = List(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity, 1.5)
    val json = JsonStringOutput.write(values)
    val read = readList(json)
    read.zip(values).foreach { case (r, v) => assert(bits(r) == bits(v), s"$v -> $r") }
  }

  test("property-based: arrays of random doubles round-trip") {
    forAll(org.scalacheck.Gen.listOf(arbitrary[Double].suchThat(java.lang.Double.isFinite))) { values =>
      val json = values.map(_.toString).mkString("[", ",", "]")
      val read = readList(json)
      assert(read.length == values.length)
      read.zip(values).foreach { case (r, v) => assert(bits(r) == bits(v)) }
    }
  }

  test("massive sweep: random doubles embedded in arrays round-trip") {
    val rnd = new Random(0xb0ffe7L)
    var i = 0
    while (i < 300000) {
      val batch = List.fill(1 + rnd.nextInt(5)) {
        var d = java.lang.Double.longBitsToDouble(rnd.nextLong())
        while (!java.lang.Double.isFinite(d)) d = java.lang.Double.longBitsToDouble(rnd.nextLong())
        d
      }
      val json = batch.map(_.toString).mkString("[", ",", "]")
      readList(json).zip(batch).foreach { case (r, v) => assert(bits(r) == bits(v), s"$v -> $r in $json") }
      i += 1
    }
  }
}
