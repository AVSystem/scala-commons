package com.avsystem.commons
package serialization.json

import org.scalatest.funsuite.AnyFunSuite

/** Exhaustively exercises the `irregular` (sig == 0, binary power of two) branch of XjbDouble/XjbFloat and the parse of
  * every such value — a code path random bit sweeps essentially never sample.
  */
class PowerOfTwoConversionTest extends AnyFunSuite {

  test("all finite power-of-two doubles format+parse bit-exactly") {
    var e = -1074
    while (e <= 1023) {
      val d = Math.scalb(1.0, e)
      List(d, -d).foreach { v =>
        val sb = new java.lang.StringBuilder
        XjbDouble.appendTo(sb, v)
        val text = sb.toString
        assert(
          java.lang.Double.doubleToLongBits(java.lang.Double.parseDouble(text)) == java.lang.Double.doubleToLongBits(v),
          s"format 2^$e: $v -> '$text'",
        )
        assert(
          java.lang.Double.doubleToLongBits(EiselLemireDouble.parse(v.toString)) == java.lang.Double.doubleToLongBits(v),
          s"parse 2^$e",
        )
        assert(
          java.lang.Double.doubleToLongBits(EiselLemireDouble.parse(text)) == java.lang.Double.doubleToLongBits(v),
          s"parse own output 2^$e: '$text'",
        )
      }
      e += 1
    }
  }

  test("all finite power-of-two floats format+parse bit-exactly") {
    var e = -149
    while (e <= 127) {
      val f = Math.scalb(1.0f, e)
      List(f, -f).foreach { v =>
        val sb = new java.lang.StringBuilder
        XjbFloat.appendTo(sb, v)
        val text = sb.toString
        assert(
          java.lang.Float.floatToIntBits(java.lang.Float.parseFloat(text)) == java.lang.Float.floatToIntBits(v),
          s"format 2^$e: $v -> '$text'",
        )
        assert(
          java.lang.Float.floatToIntBits(EiselLemireFloat.parse(v.toString)) == java.lang.Float.floatToIntBits(v),
          s"parse 2^$e",
        )
        assert(
          java.lang.Float.floatToIntBits(EiselLemireFloat.parse(text)) == java.lang.Float.floatToIntBits(v),
          s"parse own output 2^$e: '$text'",
        )
      }
      e += 1
    }
  }

  test("doubles/floats one ULP either side of each power of two also round-trip") {
    var e = -1021
    while (e <= 1023) {
      val d = Math.scalb(1.0, e)
      List(Math.nextUp(d), Math.nextDown(d)).foreach { v =>
        val sb = new java.lang.StringBuilder
        XjbDouble.appendTo(sb, v)
        assert(
          java.lang.Double.doubleToLongBits(java.lang.Double.parseDouble(sb.toString)) ==
            java.lang.Double.doubleToLongBits(v),
          s"double neighbor of 2^$e: $v -> '${sb.toString}'",
        )
      }
      e += 1
    }
    var ef = -125
    while (ef <= 127) {
      val f = Math.scalb(1.0f, ef)
      List(Math.nextUp(f), Math.nextDown(f)).foreach { v =>
        val sb = new java.lang.StringBuilder
        XjbFloat.appendTo(sb, v)
        assert(
          java.lang.Float.floatToIntBits(java.lang.Float.parseFloat(sb.toString)) == java.lang.Float.floatToIntBits(v),
          s"float neighbor of 2^$ef: $v -> '${sb.toString}'",
        )
      }
      ef += 1
    }
  }
}
