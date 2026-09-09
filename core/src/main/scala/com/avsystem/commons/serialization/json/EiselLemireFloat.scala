// The fast-path Float parsing in this file (tryDecToFloatWithFastAlgorithm / the truncated variant) is ported from
// Werner Randelshofer's FastDoubleParser (https://github.com/wrandelshofer/FastDoubleParser). It implements the
// Eisel-Lemire algorithm with the Mushtak-Lemire "Fast Number Parsing Without Fallback" refinement, reusing the
// power-of-ten table from EiselLemireDouble. On the rare ambiguous case the fast path returns NaN and this parser
// falls back to java.lang.Float.parseFloat, so results are always identical to the platform parser.
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

/** Fast, correctly-rounded `String` -> `Float` parser used by [[JsonStringInput]]. Uses the Eisel-Lemire fast path
  * (Clinger fast path + 64-bit product) and falls back to `java.lang.Float.parseFloat` for the rare hard cases (and any
  * input the fast decimal parser does not recognize), so the returned value is always exactly what
  * `java.lang.Float.parseFloat` would return.
  */
private[json] object EiselLemireFloat {
  private final val SIGNIFICAND_WIDTH = 24
  private final val EXPONENT_BIAS = 127
  private final val MAX_EXPONENT_POWER_OF_TWO = 127 // java.lang.Float.MAX_EXPONENT
  private final val MIN_POWER_OF_TEN = -45
  private final val MAX_POWER_OF_TEN = 38
  private final val DOUBLE_MIN_POWER_OF_TEN = -325 // MANTISSA_64 is indexed relative to this
  private final val MINIMAL_NINETEEN_DIGIT_INTEGER = 1000000000000000000L // 10^18
  private final val MAX_EXPONENT_NUMBER = 1024
  private final val MASK_38 = (1L << 38) - 1

  // Exact powers of ten representable as Float: 10^0 .. 10^10.
  private final val POWERS_OF_TEN: Array[Float] =
    Array(1e0f, 1e1f, 1e2f, 1e3f, 1e4f, 1e5f, 1e6f, 1e7f, 1e8f, 1e9f, 1e10f)

  /** Parses `s` as a `Float`, exactly as `java.lang.Float.parseFloat` would (Eisel-Lemire fast path, falling back to
    * `java.lang.Float.parseFloat` for anything the fast grammar can't handle, e.g. "Infinity"/"NaN").
    */
  def parse(s: String): Float = {
    val len = s.length
    var index = 0
    var isNegative = false
    if (index < len && s.charAt(index) == '-') { isNegative = true; index += 1 }

    var significand = 0L
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
    illegal |= (digitCount == 0 && significandEndIndex > sigStart)

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

    if (illegal || digitCount == 0 || index < len) {
      return java.lang.Float.parseFloat(s)
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
        } else j += 1
      }
      isSignificandTruncated = j < significandEndIndex
      exponentOfTruncatedSignificand = integerDigitCount - truncatedDigitCount + expNumber
    }

    val result =
      tryDecToFloatTruncated(isNegative, significand, exponent, isSignificandTruncated, exponentOfTruncatedSignificand)
    if (java.lang.Float.isNaN(result)) java.lang.Float.parseFloat(s) else result
  }

  private def tryDecToFloatTruncated(
    isNegative: Boolean,
    significand: Long,
    exponent: Int,
    isSignificandTruncated: Boolean,
    exponentOfTruncatedSignificand: Int,
  ): Float = {
    if (significand == 0) return if (isNegative) -0.0f else 0.0f
    if (isSignificandTruncated) {
      if (MIN_POWER_OF_TEN <= exponentOfTruncatedSignificand && exponentOfTruncatedSignificand <= MAX_POWER_OF_TEN) {
        val withoutRounding = tryDecToFloatWithFastAlgorithm(isNegative, significand, exponentOfTruncatedSignificand)
        val roundedUp = tryDecToFloatWithFastAlgorithm(isNegative, significand + 1, exponentOfTruncatedSignificand)
        if (roundedUp == withoutRounding) return withoutRounding // NaN != NaN, so also bails when both are NaN
      }
      Float.NaN
    } else if (MIN_POWER_OF_TEN <= exponent && exponent <= MAX_POWER_OF_TEN) {
      tryDecToFloatWithFastAlgorithm(isNegative, significand, exponent)
    } else Float.NaN
  }

  /** Eisel-Lemire fast path for `Float`. Returns the computed float, or `Float.NaN` when the fast path cannot guarantee
    * a correctly-rounded result. `significand` is treated as unsigned.
    */
  private def tryDecToFloatWithFastAlgorithm(isNegative: Boolean, significand: Long, power: Int): Float = {
    if (
      -10 <= power && power <= 10 && java.lang.Long.compareUnsigned(significand, (1L << SIGNIFICAND_WIDTH) - 1) <= 0
    ) {
      var d = significand.toFloat
      if (power < 0) d = d / POWERS_OF_TEN(-power) else d = d * POWERS_OF_TEN(power)
      return if (isNegative) -d else d
    }

    val factorMantissa = EiselLemireDouble.MANTISSA_64(power - DOUBLE_MIN_POWER_OF_TEN)
    val exponent = (((152170L + 65536L) * power) >> 16) + EXPONENT_BIAS + 64
    val lz0 = java.lang.Long.numberOfLeadingZeros(significand)
    val shiftedSignificand = significand << lz0
    val upper = EiselLemireDouble.unsignedMultiplyHigh(shiftedSignificand, factorMantissa)

    val upperbit = upper >>> 63
    var mantissa = upper >>> (upperbit + 38)
    var lz = lz0 + (1 ^ upperbit).toInt

    if (((upper & MASK_38) == MASK_38) || ((upper & MASK_38) == 0 && (mantissa & 3) == 1)) {
      return Float.NaN
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
      return Float.NaN
    }

    val bits = (mantissa | (realExponent << (SIGNIFICAND_WIDTH - 1)) | (if (isNegative) 1L << 31 else 0L)).toInt
    java.lang.Float.intBitsToFloat(bits)
  }
}
