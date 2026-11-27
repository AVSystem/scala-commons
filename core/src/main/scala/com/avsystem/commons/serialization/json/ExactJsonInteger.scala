package com.avsystem.commons
package serialization.json

/** Exact conversion of a JSON number's text to a whole number, used by [[JsonIntegerReading.Exact]].
  *
  * Works on the text alone, in time linear in its length and without allocating on success. It never builds a `Double`
  * (which would round) or an unlimited-precision number (which would hang or exhaust memory on inputs like
  * `1e999999999`).
  */
private[json] object ExactJsonInteger {

  /** The exponent is saturated at this magnitude. Any non-zero value with a larger exponent is out of `Long` range or
    * not whole anyway: a `String` has fewer than 2^31 digits, so they cannot compensate for 2^40 powers of ten.
    */
  private final val MaxExponent = 1L << 40

  /** The exact value of `str` as a `Long`.
    *
    * Accepted grammar (whole string): `[+-]? digit+ ('.' digit+)? ([eE] [+-]? digit+)?` with ASCII digits only; leading
    * zeros are allowed.
    *
    * @throws NumberFormatException
    *   when `str` does not follow the grammar, its value is not a whole number, or it does not fit in a `Long`
    */
  def parseLong(str: String): Long = {
    def fail(): Nothing = throw new NumberFormatException(str)

    val len = str.length
    def isDigitAt(i: Int): Boolean = i < len && {
      val c = str.charAt(i)
      c >= '0' && c <= '9'
    }
    def isSignAt(i: Int): Boolean = i < len && {
      val c = str.charAt(i)
      c == '-' || c == '+'
    }
    def digitsEnd(start: Int): Int = {
      var end = start
      while (isDigitAt(end)) end += 1
      end
    }

    // 1. validate the grammar, recording where the parts are
    val negative = len > 0 && str.charAt(0) == '-'
    val intStart = if (isSignAt(0)) 1 else 0
    val intEnd = digitsEnd(intStart)
    if (intEnd == intStart) fail()

    var i = intEnd
    var fracDigits = 0
    if (i < len && str.charAt(i) == '.') {
      val fracEnd = digitsEnd(i + 1)
      fracDigits = fracEnd - (i + 1)
      if (fracDigits == 0) fail()
      i = fracEnd
    }

    // 2. exponent, saturated so that its arithmetic cannot overflow however many digits it has
    var exponent = 0L
    if (i < len && (str.charAt(i) == 'e' || str.charAt(i) == 'E')) {
      i += 1
      val expNegative = i < len && str.charAt(i) == '-'
      if (isSignAt(i)) i += 1
      val expStart = i
      while (isDigitAt(i)) {
        exponent = math.min(exponent * 10 + (str.charAt(i) - '0'), MaxExponent)
        i += 1
      }
      if (i == expStart) fail()
      if (expNegative) exponent = -exponent
    }
    if (i != len) fail()

    // 3. the value is D * 10^scale, where D are the integer digits followed by the fraction digits
    val intDigits = intEnd - intStart
    val digitCount = intDigits + fracDigits
    // the fraction digits start one char after the integer digits, past the '.'
    def digitAt(j: Int): Int = str.charAt(if (j < intDigits) intStart + j else intStart + j + 1) - '0'

    // 4. leading zeros of D do not change the value; if D is all zeros, the value is 0 whatever the exponent
    var first = 0
    while (first < digitCount && digitAt(first) == 0) first += 1
    if (first == digitCount) 0L
    else {
      // 5. move trailing zeros of D into the scale, so that a negative scale means the value is not whole
      var last = digitCount
      var scale = exponent - fracDigits
      while (digitAt(last - 1) == 0) {
        last -= 1
        scale += 1
      }
      // 6. not a whole number
      if (scale < 0) fail()
      // 7. more than 19 digits: out of Long range (this also bounds the loops below)
      if (last - first + scale > 19) fail()

      // 8. accumulate negated, because Long.MinValue has no positive counterpart
      val multMin = Long.MinValue / 10
      var negated = 0L
      var j = first
      while (j < last) {
        val d = digitAt(j)
        if (negated < multMin) fail()
        negated *= 10
        if (negated < Long.MinValue + d) fail()
        negated -= d
        j += 1
      }
      while (scale > 0) {
        if (negated < multMin) fail()
        negated *= 10
        scale -= 1
      }
      if (negative) negated
      else if (negated == Long.MinValue) fail()
      else -negated
    }
  }
}
