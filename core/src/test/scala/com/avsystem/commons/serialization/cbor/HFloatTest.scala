package com.avsystem.commons
package serialization.cbor

import org.scalactic.source.Position
import org.scalatest.funsuite.AnyFunSuite

class HFloatTest extends AnyFunSuite {
  def testConv(name: String)(floatBits: Int, hfloatBits: Int, roundedBits: Int)(implicit pos: Position): Unit = {
    val float = java.lang.Float.intBitsToFloat(floatBits)
    test(name) {
      val hfloat = HFloat.fromFloat(float)
      assert(hfloat.raw.toInt.toHexString == hfloatBits.toInt.toHexString)
      assert(java.lang.Float.floatToIntBits(hfloat.toFloat).toHexString == roundedBits.toHexString)
    }
  }

  private def mkHFloat(exponent: Int, significand: Int): Int = ((exponent + 15) << 10) | significand

  private def mkFloat(exponent: Int, significand: Int): Int = ((exponent + 127) << 23) | significand

  testConv("zero")(0x00000000, 0x0000, 0x00000000)
  testConv("-zero")(0x80000000, 0x8000, 0x80000000)
  testConv("Inf")(0x7f800000, 0x7c00, 0x7f800000)
  testConv("-Inf")(0xff800000, 0xfc00, 0xff800000)
  testConv("1.0")(mkFloat(0, 0), mkHFloat(0, 0), mkFloat(0, 0))
  testConv("2.0")(mkFloat(1, 0), mkHFloat(1, 0), mkFloat(1, 0))
  testConv("very small float")(0x00000001, 0x0000, 0x00000000)
  testConv("rounding down")(mkFloat(0, 0xfff), mkHFloat(0, 0), mkFloat(0, 0))
  testConv("rounding up")(mkFloat(0, 0x1000), mkHFloat(0, 0x1), mkFloat(0, 0x2000))
  testConv("rounding up exponent")(mkFloat(0, 0x7fff000), mkHFloat(1, 0), mkFloat(1, 0))
  testConv("subnormal")(mkFloat(-15, 0), mkHFloat(-15, 0x200), mkFloat(-15, 0))
  testConv("subnormal with rounding")(mkFloat(-16, 0x7fe000), mkHFloat(-15, 0x200), mkFloat(-15, 0))
  testConv("subnormal with exp rounding")(mkFloat(-15, 0x7fe000), mkHFloat(-14, 0), mkFloat(-14, 0))
}
