package com.avsystem.commons
package mongo

import org.bson.types.Decimal128

import java.math.MathContext
import _root_.scala.util.control.NoStackTrace

object Decimal128Utils {
  final val MaxExponent = 6111
  final val MinExponent = -6176
  final val ExponentOffset = 6176
  final val MaxBitLength = 113
  final val SignBitMask = 1L << 63
  final val BigIntZero = new JBigInteger("0")
  final val BigIntOne = new JBigInteger("1")
  final val BigIntTen = new JBigInteger("10")

  def fromBigDecimal(value: BigDecimal): Opt[Decimal128] =
    try fromBigDecimal(value.bigDecimal, value.signum == -1).opt catch {
      case Decimal128FormatException => Opt.Empty
    }

  private final val Decimal128FormatException = new RuntimeException with NoStackTrace

  /*
   * All of the code below is copied from `org.bson.types.Decimal128` and modified to use exceptions without
   * stack traces. This is necessary for performant checking of whether a `BigDecimal` can be represented
   * as `Decimal128`.
   */

  // isNegative is necessary to detect -0, which can't be represented with a BigDecimal
  private def fromBigDecimal(initialValue: JBigDecimal, isNegative: Boolean): Decimal128 = {
    var localHigh: Long = 0
    var localLow: Long = 0

    val value = clampAndRound(initialValue)
    val exponent = -value.scale

    if (exponent < MinExponent || exponent > MaxExponent || value.unscaledValue.bitLength > MaxBitLength)
      throw Decimal128FormatException

    val significand = value.unscaledValue.abs
    val bitLength = significand.bitLength

    var i = 0
    while (i < Math.min(64, bitLength)) {
      if (significand.testBit(i)) {
        localLow |= 1L << i
      }
      i += 1
    }

    i = 64
    while (i < bitLength) {
      if (significand.testBit(i)) {
        localHigh |= 1L << (i - 64)
      }
      i += 1
    }

    val biasedExponent: Long = exponent + ExponentOffset
    localHigh |= biasedExponent << 49

    if (value.signum == -1 || isNegative) {
      localHigh |= SignBitMask
    }

    Decimal128.fromIEEE754BIDEncoding(localHigh, localLow)
  }

  private def clampAndRound(initialValue: JBigDecimal): JBigDecimal = {
    var value: JBigDecimal = null
    if (-initialValue.scale > MaxExponent) {
      val diff = -initialValue.scale - MaxExponent
      if (initialValue.unscaledValue == BigIntZero) {
        value = new JBigDecimal(initialValue.unscaledValue, -MaxExponent)
      } else if (diff + initialValue.precision > 34) {
        throw Decimal128FormatException
      } else {
        val multiplier = BigIntTen.pow(diff)
        value = new JBigDecimal(initialValue.unscaledValue.multiply(multiplier), initialValue.scale + diff)
      }
    }
    else if (-initialValue.scale < MinExponent) {
      // Increasing a very negative exponent may require decreasing precision, which is rounding
      // Only round exactly (by removing precision that is all zeroes).  An exception is thrown if the rounding would be inexact:
      // Exact:     .000...0011000  => 11000E-6177  => 1100E-6176  => .000001100
      // Inexact:   .000...0011001  => 11001E-6177  => 1100E-6176  => .000001100
      val diff = initialValue.scale + MinExponent
      val undiscardedPrecision = ensureExactRounding(initialValue, diff)
      val divisor =
        if (undiscardedPrecision == 0) BigIntOne else BigIntTen.pow(diff)
      value = new JBigDecimal(initialValue.unscaledValue.divide(divisor), initialValue.scale - diff)
    }
    else {
      value = initialValue.round(MathContext.DECIMAL128)
      val extraPrecision = initialValue.precision - value.precision
      if (extraPrecision > 0) { // Again, only round exactly
        ensureExactRounding(initialValue, extraPrecision)
      }
    }
    value
  }

  private def ensureExactRounding(initialValue: JBigDecimal, extraPrecision: Int): Int = {
    val significand = initialValue.unscaledValue.abs.toString
    val undiscardedPrecision = Math.max(0, significand.length - extraPrecision)
    var i = undiscardedPrecision
    while (i < significand.length) {
      if (significand.charAt(i) != '0') {
        throw Decimal128FormatException
      }
      i += 1
    }
    undiscardedPrecision
  }
}
