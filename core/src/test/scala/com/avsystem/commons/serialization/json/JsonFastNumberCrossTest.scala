package com.avsystem.commons
package serialization.json

import org.scalatest.funsuite.AnyFunSuite

import scala.util.Random

/** Cross-platform (JVM + Scala.js) smoke test of the fast JSON number handling — the Eisel-Lemire `Double`/`Float`
  * reads and shortest-digit writes that [[JsonStringInput]] / [[JsonStringOutput]] use. The exhaustive suites (Paxson
  * stress vectors, 2M-value sweeps, `toString`-format comparisons) are JVM-only under `core/jvm/src/test` — they are too
  * slow under Scala.js' emulated `Long` arithmetic and some compare against JVM `toString` formatting. This suite sticks
  * to platform-independent oracles: a write must round-trip bit-exactly through a read, and integer reads must preserve
  * their value.
  */
class JsonFastNumberCrossTest extends AnyFunSuite {

  private def dbits(d: Double): Long = java.lang.Double.doubleToLongBits(d)
  private def fbits(f: Float): Int = java.lang.Float.floatToIntBits(f)

  test("doubles round-trip bit-exactly through write and read") {
    val rnd = new Random(0xcafe)
    val specials = List(
      0.0,
      -0.0,
      1.0,
      -1.0,
      0.5,
      0.1,
      3.141592653589793,
      1e7,
      1e-3,
      1e300,
      1e-300,
      java.lang.Double.MAX_VALUE,
      java.lang.Double.MIN_VALUE,
      java.lang.Double.MIN_NORMAL,
    )
    val randoms = List.fill(2000)(java.lang.Double.longBitsToDouble(rnd.nextLong())).filter(java.lang.Double.isFinite)
    (specials ++ randoms).foreach { d =>
      val json = JsonStringOutput.write(d)
      assert(dbits(JsonStringInput.read[Double](json)) == dbits(d), s"$d -> '$json'")
    }
  }

  test("floats round-trip bit-exactly through write and read") {
    val rnd = new Random(0xf00d)
    val specials = List(
      0.0f,
      -0.0f,
      1.0f,
      -1.0f,
      0.5f,
      0.1f,
      3.14f,
      1e7f,
      1e-3f,
      java.lang.Float.MAX_VALUE,
      java.lang.Float.MIN_VALUE,
      java.lang.Float.MIN_NORMAL,
    )
    val randoms = List.fill(2000)(java.lang.Float.intBitsToFloat(rnd.nextInt())).filter(java.lang.Float.isFinite)
    (specials ++ randoms).foreach { f =>
      val json = JsonStringOutput.write(f)
      assert(fbits(JsonStringInput.read[Float](json)) == fbits(f), s"$f -> '$json'")
    }
  }

  test("non-finite doubles and floats write quoted and read back") {
    List(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity).foreach { d =>
      val json = JsonStringOutput.write(d)
      assert(json.startsWith("\"") && json.endsWith("\""), s"non-finite $d must be quoted: $json")
      assert(dbits(JsonStringInput.read[Double](json)) == dbits(d))
    }
    List(Float.NaN, Float.PositiveInfinity, Float.NegativeInfinity).foreach { f =>
      val json = JsonStringOutput.write(f)
      assert(json.startsWith("\"") && json.endsWith("\""), s"non-finite $f must be quoted: $json")
      assert(fbits(JsonStringInput.read[Float](json)) == fbits(f))
    }
  }

  test("integer reads round-trip") {
    val rnd = new Random(0xbeef)
    val ints = List(0, 1, -1, Int.MaxValue, Int.MinValue) ++ List.fill(2000)(rnd.nextInt())
    assert(JsonStringInput.read[List[Int]](ints.mkString("[", ",", "]")) == ints)

    val longs = List(0L, 1L, -1L, Long.MaxValue, Long.MinValue) ++ List.fill(2000)(rnd.nextLong())
    assert(JsonStringInput.read[List[Long]](longs.mkString("[", ",", "]")) == longs)

    val bytes = List[Byte](0, 1, -1, Byte.MaxValue, Byte.MinValue)
    assert(JsonStringInput.read[List[Byte]](bytes.mkString("[", ",", "]")) == bytes)

    val shorts = List[Short](0, 1, -1, Short.MaxValue, Short.MinValue)
    assert(JsonStringInput.read[List[Short]](shorts.mkString("[", ",", "]")) == shorts)
  }

  test("integer-valued non-integer literals read as integers, overflow is rejected") {
    val doc = "[1.0,2e1,50e-1]"
    assert(JsonStringInput.read[List[Int]](doc) == List(1, 20, 5))
    assert(JsonStringInput.read[List[Long]](doc) == List(1L, 20L, 5L))

    def rejects[T: serialization.GenCodec](json: String): Unit =
      assert(Try(JsonStringInput.read[List[T]](json)).isFailure, s"must reject $json")
    rejects[Int]("[3000000000]")
    rejects[Byte]("[300]")
    rejects[Short]("[70000]")
    rejects[Long]("[99999999999999999999]")
  }

  test("a mixed object of all number types round-trips") {
    val json = JsonStringOutput.write(Map("a" -> 1.5, "b" -> -0.25, "c" -> 1e10, "d" -> 3.0))
    val read = JsonStringInput.read[Map[String, Double]](json)
    assert(read == Map("a" -> 1.5, "b" -> -0.25, "c" -> 1e10, "d" -> 3.0))
  }
}
