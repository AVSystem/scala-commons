package com.avsystem.commons
package serialization.json

import com.avsystem.commons.serialization.GenCodec
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** The buffer-based integer reads must be exactly value-preserving. Numbers are read embedded in JSON arrays so the
  * parsed ranges have non-zero offsets. The oracle is round-trip fidelity (a formatted value reads back to itself) plus
  * explicit expectations for the integer-valued-decimal and overflow edge cases.
  */
class JsonReadIntegerBufferTest extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {

  /** Asserts that `values`, formatted into a JSON array, reads back identically. */
  private def assertRoundTrips[T: GenCodec](values: List[T]): Unit =
    JsonStringInput.read[List[T]](values.mkString("[", ",", "]")) shouldEqual values

  test("ints embedded in arrays round-trip") {
    val values = List(0, 1, -1, 42, -42, Int.MaxValue, Int.MinValue, 2147483646, -2147483647, 1000000, -999999)
    assertRoundTrips(values)
    values.foreach(v => assert(JsonStringInput.read[Int](v.toString) == v))
  }

  test("longs embedded in arrays round-trip") {
    val values =
      List(0L, 1L, -1L, Long.MaxValue, Long.MinValue, 9223372036854775806L, -9223372036854775807L, 123456789012345L)
    assertRoundTrips(values)
    values.foreach(v => assert(JsonStringInput.read[Long](v.toString) == v))
  }

  test("bytes and shorts round-trip incl. boundaries") {
    assertRoundTrips(List[Byte](0, 1, -1, Byte.MaxValue, Byte.MinValue, 100, -100))
    assertRoundTrips(List[Short](0, 1, -1, Short.MaxValue, Short.MinValue, 30000, -30000))
  }

  test("integer-valued non-integer literals (1.0, 2e1) read as their integer value") {
    JsonStringInput.read[List[Int]]("[1.0,2e1,-3,4.0e0,50e-1]") shouldEqual List(1, 20, -3, 4, 5)
    JsonStringInput.read[List[Long]]("[1.0,2e1,-3,4.0e0,50e-1]") shouldEqual List(1L, 20L, -3L, 4L, 5L)
  }

  test("overflow and out-of-range are rejected") {
    intercept[Exception](JsonStringInput.read[List[Int]]("[3000000000]")) // > Int.MaxValue
    intercept[Exception](JsonStringInput.read[List[Int]]("[-3000000000]"))
    intercept[Exception](JsonStringInput.read[List[Long]]("[99999999999999999999]")) // > Long.MaxValue
    intercept[Exception](JsonStringInput.read[List[Byte]]("[300]"))
    intercept[Exception](JsonStringInput.read[List[Short]]("[70000]"))
  }

  test("property-based: random int arrays round-trip") {
    forAll(org.scalacheck.Gen.listOf(arbitrary[Int])) { xs =>
      assertRoundTrips(xs)
    }
  }

  test("property-based: random long arrays round-trip") {
    forAll(org.scalacheck.Gen.listOf(arbitrary[Long])) { xs =>
      assertRoundTrips(xs)
    }
  }

  test("massive sweep: random ints/longs read bit-exactly via the buffer path") {
    val rnd = new scala.util.Random(0x1d7e6e7L)
    var i = 0
    while (i < 500000) {
      val iv = rnd.nextInt()
      assert(JsonStringInput.read[Int](iv.toString) == iv)
      val lv = rnd.nextLong()
      assert(JsonStringInput.read[Long](lv.toString) == lv)
      i += 1
    }
  }
}
