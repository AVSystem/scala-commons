// This file ports the *numeric core* of the "xjb" fast float-to-string algorithm by xjb714 and contributors
// (https://github.com/xjb714/xjb), Copyright 2026 xjb714 and contributors, licensed under the Apache License,
// Version 2.0 (http://www.apache.org/licenses/LICENSE-2.0).
//
// Scope of the port: xjb is a SIMD-first C++ library; its speed comes from NEON/SSE/AVX-512 code that a JVM hand-port
// cannot reproduce. This file ports only xjb's *scalar* shortest-decimal computation (the h7 / pow10 tables and the
// single-128-bit-multiply rounding that yields the shortest significand `m_up*10 + (up_down?0:one)` and exponent `k`).
// The digit layout is NOT xjb's (that part is pure SIMD); instead the shortest significand is formatted with a
// java.lang.Double.toString-style layout. Even in scalar form this beats an iterative-digit-removal formatter (Ryū) on
// the JVM; it is still NOT representative of xjb's real C++ (SIMD) performance.
package com.avsystem.commons
package serialization.json

/** Fast shortest-`Double` formatter used by [[JsonStringOutput]]: a scalar port of the xjb numeric core (see file
  * header for scope/caveats). Always produces text that parses back to the exact same `Double`.
  */
private[json] object XjbDouble {
  final val MaxChars = 25

  private final val NUM_POW10 = 323 - -293 + 1 // 617
  private final val POW10_BASE = 323 * 2 + 2 // index of e10=0 hi word within pow10Double
  // pow10Double[i*2], pow10Double[i*2+1] hold the (hi, lo) 128-bit normalized mantissa of a power of ten.
  private final val pow10Double: Array[Long] = new Array[Long](NUM_POW10 * 2)
  private final val h7: Array[Int] = new Array[Int](2048)

  private final val C4 = (1L << 63) + 6

  // Not ThreadLocal.withInitial: that factory is absent from the Scala.js javalib.
  private[this] val scratch: ThreadLocal[Array[Char]] =
    new ThreadLocal[Array[Char]] {
      override def initialValue(): Array[Char] = new Array[Char](MaxChars)
    }

  // ---- unsigned 64-bit helpers (the low 64 bits of an unsigned product are just the wrapping product) ----

  private def umulHi(x: Long, y: Long): Long = EiselLemireDouble.unsignedMultiplyHigh(x, y)

  // (a*b + c) >>> 64
  private def maddHi(a: Long, b: Long, c: Long): Long = {
    val lo = a * b
    umulHi(a, b) + (if (java.lang.Long.compareUnsigned(lo + c, lo) < 0) 1L else 0L)
  }

  private def ugt(a: Long, b: Long): Boolean = java.lang.Long.compareUnsigned(a, b) > 0

  locally {
    // Replicate xjb's constexpr pow10 table generation (192-bit fixed point, *10 with re-normalization).
    var w0 = 0xb2e28cedd086d011L
    var w1 = 0x1e53ed49a96272c8L
    var w2 = 0xcc5fc196fefd7d0cL // e10 = -293
    val ten = 0xa000000000000000L
    var i = 0
    while (i < NUM_POW10) {
      val e10 = i - 293
      pow10Double((NUM_POW10 - 1 - i) * 2 + 0) =
        if (e10 == 0) 1L << 63 else w2 + (if (e10 >= 0 && e10 <= 27) 1L else 0L)
      pow10Double((NUM_POW10 - 1 - i) * 2 + 1) = w1 + 1
      val h0 = umulHi(w0, ten)
      val h1 = umulHi(w1, ten)
      val c0 = h0 + w1 * ten
      val c1 = (if (ugt(h0, c0)) 1L else 0L) + h1 + w2 * ten
      val c2 = (if (ugt(h1, c1)) 1L else 0L) + umulHi(w2, ten)
      if ((c2 >>> 63) != 0) { w0 = c0; w1 = c1; w2 = c2 }
      else { w0 = c0 << 1; w1 = (c1 << 1) | (c0 >>> 63); w2 = (c2 << 1) | (c1 >>> 63) }
      i += 1
    }
    var exp = 0
    while (exp < 2048) {
      val offset = 9
      val q = exp - 1075 + (if (exp == 0) 1 else 0)
      val k = (q * 78913) >> 18
      val h = q + (((-k - 1) * 217707) >> 16)
      h7(exp) = h + 1 + offset
      exp += 1
    }
  }

  private def getPow10Hi(k: Int): Long = pow10Double(POW10_BASE + k * 2)
  private def getPow10Lo(k: Int): Long = pow10Double(POW10_BASE + k * 2 + 1)

  /** Formats the finite `value` and appends the result to `builder`. NaN/Infinity must be handled by the caller. */
  def appendTo(builder: java.lang.StringBuilder, value: Double): Unit = {
    val buf = scratch.get()
    builder.append(buf, 0, toChars(value, buf, 0))
  }

  def toChars(value: Double, out: Array[Char], offset: Int): Int = {
    val vi = java.lang.Double.doubleToLongBits(value)
    val sign = vi < 0
    val sig = vi & ((1L << 52) - 1)
    val exp = ((vi << 1) >>> 53).toInt

    var index = offset
    if (sign) { out(index) = '-'; index += 1 }

    // 0.0 / -0.0 and the smallest subnormal are special-cased (xjb renders 4.9e-324 as "5e-324").
    if (exp == 0 && sig <= 1) {
      if (sig == 0) { out(index) = '0'; out(index + 1) = '.'; out(index + 2) = '0'; return index + 3 - offset }
      out(index) = '5'; out(index + 1) = 'e'; out(index + 2) = '-'
      out(index + 3) = '3'; out(index + 4) = '2'; out(index + 5) = '4'
      return index + 6 - offset
    }

    var c = sig | (1L << 52)
    var q = exp.toLong - 1075
    if (exp == 0) { c = sig; q = 1 - 1075 }
    val h7p = h7(exp)
    val offset9 = 9
    val irregular = sig == 0

    var mUp = 0L
    var one = 0L
    var upDown = 0
    var k = 0
    if (!irregular) {
      k = ((q * 78913L) >> 18).toInt
      val pow10Hi = getPow10Hi(k)
      val pow10Lo = getPow10Lo(k)
      // top 128 bits of {pow10Hi:pow10Lo} * cb: hi64 is the high word, lo64 the middle word
      val cb = c << h7p
      val lowHigh = umulHi(pow10Lo, cb)
      val lo64 = lowHigh + pow10Hi * cb
      val hi64 = umulHi(pow10Hi, cb) + (if (ugt(lowHigh, lo64)) 1L else 0L)
      val dotOne = (hi64 << (64 - offset9)) | (lo64 >>> offset9)
      val halfUlp = (pow10Hi >>> ((1 + offset9) - h7p)) + ((c + 1) & 1)
      val up = ugt(halfUlp, ~dotOne)
      val down = ugt(halfUlp, dotOne)
      mUp = (hi64 >>> offset9) + (if (up) 1L else 0L)
      upDown = (if (up) 1 else 0) + (if (down) 1 else 0)
      val half = if (dotOne == (1L << 62)) 0L else C4
      one = maddHi(dotOne, 10L, half)
    } else {
      k = ((q * 315653L - 131072L) >> 20).toInt
      val h = (q + ((k.toLong * -217707L - 217707L) >> 16)).toInt
      val pow10Hi = getPow10Hi(k)
      val halfUlp = pow10Hi >>> (-h)
      val dotOne = pow10Hi << (53 + h)
      val up = ugt(halfUlp, ~dotOne)
      val down = ugt(halfUlp >>> 1, dotOne)
      mUp = (pow10Hi >>> (11 - h)) + (if (up) 1L else 0L)
      upDown = (if (up) 1 else 0) + (if (down) 1 else 0)
      one = ((dotOne >>> (53 + h)) * 5 + (1L << (9 - h))) >>> (10 - h)
      if ((((dotOne >>> 54) * 5) & ((1 << 9) - 1)) > ((halfUlp >>> 55) * 5))
        one = (((dotOne >>> 54) * 5) >>> 9) + 1
      if (dotOne == (1L << 62)) one = 2
    }

    // value = shortestSig * 10^k  (xjb's stated invariant)
    var shortestSig = mUp * 10 + (if (upDown != 0) 0L else one)
    var k10 = k
    // strip trailing zeros to get the minimal digit sequence
    while (shortestSig % 10 == 0) { shortestSig /= 10; k10 += 1 }

    val nd = decimalLength(shortestSig)
    val e = k10 + nd - 1 // exponent of the leading digit

    index += writeShortest(out, index, shortestSig, nd, e)
    index - offset
  }

  /** Java.lang.Double.toString-style layout of `output` (a `nd`-digit significand with no trailing zeros) whose leading
    * digit has decimal exponent `exp`. Lowercase 'e' (SenML). Returns the number of chars written at `off`.
    */
  private[json] def writeShortest(out: Array[Char], off: Int, output0: Long, olength: Int, exp0: Int): Int = {
    var output = output0
    var exp = exp0
    var index = 0
    val scientificNotation = !((exp >= -3) && (exp < 7))
    if (scientificNotation) {
      var i = 0
      while (i < olength - 1) {
        val d = (output % 10).toInt
        output /= 10
        out(off + index + olength - i) = ('0' + d).toChar
        i += 1
      }
      out(off + index) = ('0' + (output % 10)).toChar
      out(off + index + 1) = '.'
      index += olength + 1
      if (olength == 1) { out(off + index) = '0'; index += 1 }
      out(off + index) = 'e'; index += 1
      if (exp < 0) { out(off + index) = '-'; index += 1; exp = -exp }
      if (exp >= 100) {
        out(off + index) = ('0' + exp / 100).toChar; index += 1
        exp %= 100
        out(off + index) = ('0' + exp / 10).toChar; index += 1
      } else if (exp >= 10) {
        out(off + index) = ('0' + exp / 10).toChar; index += 1
      }
      out(off + index) = ('0' + exp % 10).toChar; index += 1
      index
    } else if (exp < 0) {
      out(off + index) = '0'; index += 1
      out(off + index) = '.'; index += 1
      var i = -1
      while (i > exp) { out(off + index) = '0'; index += 1; i -= 1 }
      val current = index
      var kk = 0
      while (kk < olength) {
        out(off + current + olength - kk - 1) = ('0' + output % 10).toChar
        output /= 10
        index += 1
        kk += 1
      }
      index
    } else if (exp + 1 >= olength) {
      var kk = 0
      while (kk < olength) {
        out(off + index + olength - kk - 1) = ('0' + output % 10).toChar
        output /= 10
        kk += 1
      }
      index += olength
      var i = olength
      while (i < exp + 1) { out(off + index) = '0'; index += 1; i += 1 }
      out(off + index) = '.'; index += 1
      out(off + index) = '0'; index += 1
      index
    } else {
      var current = index + 1
      var kk = 0
      while (kk < olength) {
        if (olength - kk - 1 == exp) { out(off + current + olength - kk - 1) = '.'; current -= 1 }
        out(off + current + olength - kk - 1) = ('0' + output % 10).toChar
        output /= 10
        kk += 1
      }
      index += olength + 1
      index
    }
  }

  private[json] def decimalLength(v: Long): Int =
    if (v >= 1000000000000000000L) 19
    else if (v >= 100000000000000000L) 18
    else if (v >= 10000000000000000L) 17
    else if (v >= 1000000000000000L) 16
    else if (v >= 100000000000000L) 15
    else if (v >= 10000000000000L) 14
    else if (v >= 1000000000000L) 13
    else if (v >= 100000000000L) 12
    else if (v >= 10000000000L) 11
    else if (v >= 1000000000L) 10
    else if (v >= 100000000L) 9
    else if (v >= 10000000L) 8
    else if (v >= 1000000L) 7
    else if (v >= 100000L) 6
    else if (v >= 10000L) 5
    else if (v >= 1000L) 4
    else if (v >= 100L) 3
    else if (v >= 10L) 2
    else 1
}
