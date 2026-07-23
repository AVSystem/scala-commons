package com.avsystem.commons
package serialization.json

import org.scalatest.funsuite.AnyFunSuite

import scala.util.Random

/** Cross-platform (JVM + Scala.js) smoke test of [[JsonNumberCodec.Fast]]. The exhaustive suites (Paxson stress
  * vectors, 2M-value sweeps, `toString`-format comparisons) are JVM-only under `core/jvm/src/test` — they are too slow
  * under Scala.js' emulated `Long` arithmetic and some compare against JVM `toString` formatting. This suite sticks to
  * platform-independent oracles: Fast write -> Fast read must round-trip bit-exactly, and Fast reads must agree with
  * Standard reads of the same document.
  */
class JsonNumberCodecCrossTest extends AnyFunSuite {

  private val fast = JsonOptions(numberCodec = JsonNumberCodec.Fast)
  private val std = JsonOptions.Default

  private def dbits(d: Double): Long = java.lang.Double.doubleToLongBits(d)
  private def fbits(f: Float): Int = java.lang.Float.floatToIntBits(f)

  test("doubles round-trip bit-exactly through Fast write and read") {
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
      val json = JsonStringOutput.write(d, fast)
      assert(dbits(JsonStringInput.read[Double](json, fast)) == dbits(d), s"$d -> '$json'")
      assert(dbits(JsonStringInput.read[Double](json, std)) == dbits(d), s"Standard read of '$json'")
    }
  }

  test("floats round-trip bit-exactly through Fast write and read") {
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
      val json = JsonStringOutput.write(f, fast)
      assert(fbits(JsonStringInput.read[Float](json, fast)) == fbits(f), s"$f -> '$json'")
    }
  }

  test("non-finite doubles and floats write quoted and read back in Fast mode") {
    List(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity).foreach { d =>
      val json = JsonStringOutput.write(d, fast)
      assert(json == JsonStringOutput.write(d, std))
      assert(dbits(JsonStringInput.read[Double](json, fast)) == dbits(d))
    }
    List(Float.NaN, Float.PositiveInfinity, Float.NegativeInfinity).foreach { f =>
      val json = JsonStringOutput.write(f, fast)
      assert(json == JsonStringOutput.write(f, std))
      assert(fbits(JsonStringInput.read[Float](json, fast)) == fbits(f))
    }
  }

  test("integer reads agree between Fast and Standard") {
    val rnd = new Random(0xbeef)
    val ints = List(0, 1, -1, Int.MaxValue, Int.MinValue) ++ List.fill(2000)(rnd.nextInt())
    val intJson = ints.mkString("[", ",", "]")
    assert(JsonStringInput.read[List[Int]](intJson, fast) == JsonStringInput.read[List[Int]](intJson, std))

    val longs = List(0L, 1L, -1L, Long.MaxValue, Long.MinValue) ++ List.fill(2000)(rnd.nextLong())
    val longJson = longs.mkString("[", ",", "]")
    assert(JsonStringInput.read[List[Long]](longJson, fast) == JsonStringInput.read[List[Long]](longJson, std))

    val bytes = List[Byte](0, 1, -1, Byte.MaxValue, Byte.MinValue)
    val byteJson = bytes.mkString("[", ",", "]")
    assert(JsonStringInput.read[List[Byte]](byteJson, fast) == JsonStringInput.read[List[Byte]](byteJson, std))

    val shorts = List[Short](0, 1, -1, Short.MaxValue, Short.MinValue)
    val shortJson = shorts.mkString("[", ",", "]")
    assert(JsonStringInput.read[List[Short]](shortJson, fast) == JsonStringInput.read[List[Short]](shortJson, std))
  }

  test("non-integer literals and overflow behave identically in Fast and Standard") {
    val doc = "[1.0,2e1,50e-1]"
    assert(JsonStringInput.read[List[Int]](doc, fast) == JsonStringInput.read[List[Int]](doc, std))
    assert(JsonStringInput.read[List[Long]](doc, fast) == JsonStringInput.read[List[Long]](doc, std))

    def sameFailure[T: serialization.GenCodec](json: String): Unit = {
      val fastFails = Try(JsonStringInput.read[List[T]](json, fast)).isFailure
      val stdFails = Try(JsonStringInput.read[List[T]](json, std)).isFailure
      assert(fastFails && stdFails, s"both modes must reject $json (fast=$fastFails std=$stdFails)")
    }
    sameFailure[Int]("[3000000000]")
    sameFailure[Byte]("[300]")
    sameFailure[Short]("[70000]")
    sameFailure[Long]("[99999999999999999999]")
  }

  test("a mixed object of all number types round-trips in Fast mode") {
    val json = JsonStringOutput.write(
      Map("a" -> 1.5, "b" -> -0.25, "c" -> 1e10, "d" -> 3.0),
      fast,
    )
    val read = JsonStringInput.read[Map[String, Double]](json, fast)
    assert(read == Map("a" -> 1.5, "b" -> -0.25, "c" -> 1e10, "d" -> 3.0))
  }
}
