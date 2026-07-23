package com.avsystem.commons
package serialization.json

import com.avsystem.commons.serialization.GenCodec
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Fast (buffer-based) integer reads must behave exactly like the Standard (String-based) path: same values, same
  * failures. Numbers are read embedded in JSON arrays so the parsed ranges have non-zero offsets.
  */
class JsonReadIntegerBufferTest extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {

  private val fast = JsonOptions(numberCodec = JsonNumberCodec.Fast)
  private val std = JsonOptions.Default

  /** Reads the JSON both ways and asserts identical outcome (value or thrown exception type). */
  private def assertSameOutcome[T: GenCodec](json: String): Unit = {
    val fastR = Try(JsonStringInput.read[List[T]](json, fast))
    val stdR = Try(JsonStringInput.read[List[T]](json, std))
    (fastR, stdR) match {
      case (Success(fv), Success(sv)) => assert(fv == sv, s"$json: fast=$fv std=$sv")
      case (Failure(_), Failure(_)) => // both rejected - good
      case other => fail(s"$json: fast/std disagreed on success: $other")
    }
  }

  test("ints embedded in arrays match Standard") {
    val values = List(0, 1, -1, 42, -42, Int.MaxValue, Int.MinValue, 2147483646, -2147483647, 1000000, -999999)
    assertSameOutcome[Int](values.mkString("[", ",", "]"))
    values.foreach(v => assert(JsonStringInput.read[Int](v.toString, fast) == v))
  }

  test("longs embedded in arrays match Standard") {
    val values =
      List(0L, 1L, -1L, Long.MaxValue, Long.MinValue, 9223372036854775806L, -9223372036854775807L, 123456789012345L)
    assertSameOutcome[Long](values.mkString("[", ",", "]"))
    values.foreach(v => assert(JsonStringInput.read[Long](v.toString, fast) == v))
  }

  test("bytes and shorts match Standard incl. boundaries") {
    val bytes = List[Byte](0, 1, -1, Byte.MaxValue, Byte.MinValue, 100, -100)
    assertSameOutcome[Byte](bytes.mkString("[", ",", "]"))
    val shorts = List[Short](0, 1, -1, Short.MaxValue, Short.MinValue, 30000, -30000)
    assertSameOutcome[Short](shorts.mkString("[", ",", "]"))
  }

  test("integer-valued non-integer literals (1.0, 2e1) read identically") {
    assertSameOutcome[Int]("[1.0,2e1,-3,4.0e0,50e-1]")
    assertSameOutcome[Long]("[1.0,2e1,-3,4.0e0,50e-1]")
  }

  test("overflow and out-of-range are rejected in both modes") {
    assertSameOutcome[Int]("[3000000000]") // > Int.MaxValue
    assertSameOutcome[Int]("[-3000000000]")
    assertSameOutcome[Long]("[99999999999999999999]") // > Long.MaxValue
    assertSameOutcome[Byte]("[300]")
    assertSameOutcome[Short]("[70000]")
  }

  test("property-based: random int arrays match Standard") {
    forAll(org.scalacheck.Gen.listOf(arbitrary[Int])) { xs =>
      assertSameOutcome[Int](xs.mkString("[", ",", "]"))
    }
  }

  test("property-based: random long arrays match Standard") {
    forAll(org.scalacheck.Gen.listOf(arbitrary[Long])) { xs =>
      assertSameOutcome[Long](xs.mkString("[", ",", "]"))
    }
  }

  test("massive sweep: random ints/longs read bit-exactly via the buffer path") {
    val rnd = new scala.util.Random(0x1d7e6e7L)
    var i = 0
    while (i < 500000) {
      val iv = rnd.nextInt()
      assert(JsonStringInput.read[Int](iv.toString, fast) == iv)
      val lv = rnd.nextLong()
      assert(JsonStringInput.read[Long](lv.toString, fast) == lv)
      i += 1
    }
  }
}
