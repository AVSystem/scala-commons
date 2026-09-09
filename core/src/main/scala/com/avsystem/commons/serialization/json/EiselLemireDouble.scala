// The fast-path Double parsing in this file (tryDecToDoubleWithFastAlgorithm / the truncated variant) is ported from
// Werner Randelshofer's FastDoubleParser (https://github.com/wrandelshofer/FastDoubleParser). It implements the
// Eisel-Lemire algorithm with the Mushtak-Lemire "Fast Number Parsing Without Fallback" refinement. The MANTISSA_64
// power-of-ten table is generated at class-init via BigInteger rather than transcribed. On the rare ambiguous case the
// fast path returns NaN and this parser falls back to java.lang.Double.parseDouble, so results are always identical to
// the platform parser.
//
// MIT License
//
// Copyright (c) 2024 Werner Randelshofer, Switzerland.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
// documentation files (the "Software"), to deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of
// the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
// WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
// OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
package com.avsystem.commons
package serialization.json

import java.math.BigInteger

/** Fast, correctly-rounded `String` -> `Double` parser used by [[JsonStringInput]]. Uses the Eisel-Lemire fast path and
  * falls back to `java.lang.Double.parseDouble` for the rare hard cases (and any input the fast decimal parser does not
  * recognize, e.g. "Infinity"/"NaN"), so the returned value is always exactly what `Double.parseDouble` would return.
  */
private[json] object EiselLemireDouble {
  private final val SIGNIFICAND_WIDTH = 53
  private final val EXPONENT_BIAS = 1023
  private final val MAX_EXPONENT_POWER_OF_TWO = 1023 // java.lang.Double.MAX_EXPONENT
  private final val MIN_POWER_OF_TEN = -325
  private final val MAX_POWER_OF_TEN = 308
  private final val MINIMAL_NINETEEN_DIGIT_INTEGER = 1000000000000000000L // 10^18
  private final val MAX_EXPONENT_NUMBER = 1024

  // MANTISSA_64[q - MIN_POWER_OF_TEN] = truncated (rounded down) top 64 bits of 10^q, normalized so bit 63 is set.
  private[json] final val MANTISSA_64: Array[Long] = {
    val n = MAX_POWER_OF_TEN - MIN_POWER_OF_TEN + 1
    val table = new Array[Long](n)
    val P = 1300 // enough bits so that N below has >= 64 significant bits for every q in range
    val two = BigInteger.valueOf(2)
    val ten = BigInteger.TEN
    val shift = two.pow(P)
    var q = MIN_POWER_OF_TEN
    while (q <= MAX_POWER_OF_TEN) {
      // N = floor(10^q * 2^P);  mantissa = N >> (bitLength(N) - 64)  == floor(10^q * 2^(63 - floor(log2(10^q))))
      val nn = if (q >= 0) ten.pow(q).multiply(shift) else shift.divide(ten.pow(-q))
      val mantissa = nn.shiftRight(nn.bitLength - 64)
      table(q - MIN_POWER_OF_TEN) = mantissa.longValue // low 64 bits = the 64-bit pattern (treated as unsigned)
      q += 1
    }
    table
  }

  private final val POWERS_OF_TEN: Array[Double] =
    Array(1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11, 1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19,
      1e20, 1e21, 1e22)

  private[json] def unsignedMultiplyHigh(a: Long, b: Long): Long = {
    val hi = Math.multiplyHigh(a, b)
    hi + (if (a < 0) b else 0L) + (if (b < 0) a else 0L)
  }

  /** Parses `s` as a `Double`, exactly as `java.lang.Double.parseDouble` would (Eisel-Lemire fast path, falling back to
    * `java.lang.Double.parseDouble` for anything the fast grammar can't handle, e.g. "Infinity"/"NaN").
    */
  def parse(s: String): Double = {
    val len = s.length
    var index = 0
    var isNegative = false
    if (index < len && s.charAt(index) == '-') { isNegative = true; index += 1 }

    var significand = 0L // unsigned
    val sigStart = index
    var integerDigitCount = -1
    var illegal = false
    var brk = false
    while (!brk && index < len) {
      val ch = s.charAt(index)
      val digit = ch - '0'
      if (digit >= 0 && digit < 10) { significand = 10 * significand + digit; index += 1 }
      else if (ch == '.') {
        if (integerDigitCount >= 0) illegal = true
        integerDigitCount = index - sigStart
        index += 1
      } else brk = true
    }
    val significandEndIndex = index
    val hadDot = integerDigitCount >= 0
    val digitCount = if (!hadDot) significandEndIndex - sigStart else significandEndIndex - sigStart - 1
    if (!hadDot) integerDigitCount = digitCount
    var exponent = if (!hadDot) 0 else integerDigitCount - digitCount
    illegal |= (digitCount == 0 && significandEndIndex > sigStart) // e.g. a lone "."

    // exponent part
    var expNumber = 0
    if (index < len && (s.charAt(index) | 0x20) == 'e') {
      index += 1
      var expNegative = false
      if (index < len && (s.charAt(index) == '-' || s.charAt(index) == '+')) {
        expNegative = s.charAt(index) == '-'
        index += 1
      }
      var d = if (index < len) s.charAt(index) - '0' else 99
      illegal |= !(d >= 0 && d < 10)
      while (index < len && { d = s.charAt(index) - '0'; d >= 0 && d < 10 }) {
        if (expNumber < MAX_EXPONENT_NUMBER) expNumber = 10 * expNumber + d
        index += 1
      }
      if (expNegative) expNumber = -expNumber
      exponent += expNumber
    }

    // Anything the fast decimal grammar didn't fully consume (incl. "Infinity"/"NaN", trailing junk) -> fall back.
    if (illegal || digitCount == 0 || index < len) {
      return java.lang.Double.parseDouble(s)
    }

    var isSignificandTruncated = false
    var exponentOfTruncatedSignificand = 0
    if (digitCount > 19) {
      var truncatedDigitCount = 0
      significand = 0L
      var j = sigStart
      var stop = false
      while (!stop && j < significandEndIndex) {
        val digit = s.charAt(j) - '0'
        if (digit >= 0 && digit < 10) {
          if (java.lang.Long.compareUnsigned(significand, MINIMAL_NINETEEN_DIGIT_INTEGER) < 0) {
            significand = 10 * significand + digit
            truncatedDigitCount += 1
            j += 1
          } else stop = true
        } else j += 1 // skip the '.'
      }
      isSignificandTruncated = j < significandEndIndex
      exponentOfTruncatedSignificand = integerDigitCount - truncatedDigitCount + expNumber
    }

    val result =
      tryDecToDoubleTruncated(isNegative, significand, exponent, isSignificandTruncated, exponentOfTruncatedSignificand)
    if (java.lang.Double.isNaN(result)) java.lang.Double.parseDouble(s) else result
  }

  private def tryDecToDoubleTruncated(
    isNegative: Boolean,
    significand: Long,
    exponent: Int,
    isSignificandTruncated: Boolean,
    exponentOfTruncatedSignificand: Int,
  ): Double = {
    if (significand == 0) return if (isNegative) -0.0 else 0.0
    if (isSignificandTruncated) {
      if (MIN_POWER_OF_TEN <= exponentOfTruncatedSignificand && exponentOfTruncatedSignificand <= MAX_POWER_OF_TEN) {
        val withoutRounding = tryDecToDoubleWithFastAlgorithm(isNegative, significand, exponentOfTruncatedSignificand)
        val roundedUp = tryDecToDoubleWithFastAlgorithm(isNegative, significand + 1, exponentOfTruncatedSignificand)
        if (roundedUp == withoutRounding) return withoutRounding // NaN != NaN, so also bails when both are NaN
      }
      Double.NaN
    } else if (MIN_POWER_OF_TEN <= exponent && exponent <= MAX_POWER_OF_TEN) {
      tryDecToDoubleWithFastAlgorithm(isNegative, significand, exponent)
    } else Double.NaN
  }

  /** Eisel-Lemire fast path. Returns the computed double, or `Double.NaN` when the fast path cannot guarantee a
    * correctly-rounded result. `significand` is treated as an unsigned 64-bit value. `power` must be in
    * [[MIN_POWER_OF_TEN]]..[[MAX_POWER_OF_TEN]].
    */
  private def tryDecToDoubleWithFastAlgorithm(isNegative: Boolean, significand: Long, power: Int): Double = {
    // Clinger fast path: exact when significand fits in a double and 10^|power| is exactly representable.
    if (
      -22 <= power && power <= 22 && java.lang.Long.compareUnsigned(significand, (1L << SIGNIFICAND_WIDTH) - 1) <= 0
    ) {
      var d = significand.toDouble
      if (power < 0) d = d / POWERS_OF_TEN(-power) else d = d * POWERS_OF_TEN(power)
      return if (isNegative) -d else d
    }

    val factorMantissa = MANTISSA_64(power - MIN_POWER_OF_TEN)
    val exponent = (((152170L + 65536L) * power) >> 16) + EXPONENT_BIAS + 64
    val lz0 = java.lang.Long.numberOfLeadingZeros(significand)
    val shiftedSignificand = significand << lz0
    val upper = unsignedMultiplyHigh(shiftedSignificand, factorMantissa)

    val upperbit = upper >>> 63
    var mantissa = upper >>> (upperbit + 9)
    var lz = lz0 + (1 ^ upperbit).toInt

    if (((upper & 0x1ff) == 0x1ff) || ((upper & 0x1ff) == 0 && (mantissa & 3) == 1)) {
      // Too close to a rounding boundary to decide here.
      return Double.NaN
    }

    mantissa += 1
    mantissa >>>= 1

    if (mantissa >= (1L << SIGNIFICAND_WIDTH)) {
      mantissa = 1L << (SIGNIFICAND_WIDTH - 1)
      lz -= 1
    }

    mantissa &= ~(1L << (SIGNIFICAND_WIDTH - 1))

    val realExponent = exponent - lz
    if (realExponent < 1 || realExponent > MAX_EXPONENT_POWER_OF_TWO + EXPONENT_BIAS) {
      return Double.NaN
    }

    val bits = mantissa | (realExponent << (SIGNIFICAND_WIDTH - 1)) | (if (isNegative) 1L << 63 else 0L)
    java.lang.Double.longBitsToDouble(bits)
  }
}
