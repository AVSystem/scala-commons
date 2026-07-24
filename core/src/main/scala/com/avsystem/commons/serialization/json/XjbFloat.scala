// This file ports the *numeric core* of the "xjb" fast float-to-string algorithm by xjb714 and contributors
// (https://github.com/xjb714/xjb), Copyright 2026 xjb714 and contributors, licensed under the Apache License,
// Version 2.0 (http://www.apache.org/licenses/LICENSE-2.0). See XjbDouble for the scope/caveats of the port; this is
// the `float` counterpart (a port of xjb's scalar `xjb32` core). The pure next digit `one` is recovered from xjb's
// ASCII-baked constant by dropping its ASCII part. The shortest significand is formatted with the same
// Float.toString-style layout as XjbDouble.
package com.avsystem.commons
package serialization.json

/** Fast shortest-`Float` formatter used by [[JsonStringOutput]]: a scalar port of the xjb `xjb32` numeric core. Always
  * produces text that parses back to the exact same `Float`.
  */
private[json] object XjbFloat {
  final val MaxChars = 15

  private final val NUM_POW10 = 44 - -32 + 1 // 77
  private final val pow10Reverse: Array[Long] = new Array[Long](NUM_POW10)
  private final val h37: Array[Int] = new Array[Int](256)

  // Not ThreadLocal.withInitial: that factory is absent from the Scala.js javalib.
  private[this] val scratch: ThreadLocal[Array[Char]] =
    new ThreadLocal[Array[Char]] {
      override def initialValue(): Array[Char] = new Array[Char](MaxChars)
    }

  private def ugt(a: Long, b: Long): Boolean = java.lang.Long.compareUnsigned(a, b) > 0

  locally {
    // pow10Reverse: xjb's constexpr float pow10 table (128-bit fixed point normalized, top 64 bits stored).
    var w0 = 0x67de18eda5814af3L
    var w1 = 0xcfb11ead453994baL // e10 = -32
    val ten = 0xa000000000000000L
    var i = 0
    while (i < NUM_POW10) {
      val e10 = i - 32
      pow10Reverse(NUM_POW10 - i - 1) = if (e10 == 0) 1L << 63 else w1 + 1
      val h0 = EiselLemireDouble.unsignedMultiplyHigh(w0, ten)
      val c0 = h0 + w1 * ten
      val c1 = (if (ugt(h0, c0)) 1L else 0L) + EiselLemireDouble.unsignedMultiplyHigh(w1, ten)
      if ((c1 >>> 63) != 0) { w0 = c0; w1 = c1 }
      else { w0 = c0 << 1; w1 = (c1 << 1) | (c0 >>> 63) }
      i += 1
    }
    var exp = 0
    while (exp < 256) {
      val expBin = exp - 150 + (if (exp == 0) 1 else 0)
      val k = (expBin * 1233) >> 12
      h37(exp) = 37 + expBin + ((k * -1701 + -1701) >> 9)
      exp += 1
    }
  }

  /** Formats the finite `value` and appends the result to `builder`. NaN/Infinity must be handled by the caller. */
  def appendTo(builder: java.lang.StringBuilder, value: Float): Unit = {
    val buf = scratch.get()
    builder.append(buf, 0, toChars(value, buf, 0))
  }

  def toChars(value: Float, out: Array[Char], offset: Int): Int = {
    val vi = java.lang.Float.floatToIntBits(value)
    val sign = vi < 0
    val sig = vi & ((1 << 23) - 1)
    val exp = (vi << 1) >>> 24 // biased 8-bit exponent

    var index = offset
    if (sign) { out(index) = '-'; index += 1 }
    if (exp == 0 && sig == 0) {
      out(index) = '0'; out(index + 1) = '.'; out(index + 2) = '0'
      return index + 3 - offset
    }

    var sigBin = sig | (1 << 23)
    var expBin = exp - 150
    if (exp == 0) { expBin = 1 - 150; sigBin = sig }
    var h37p = h37(exp)
    val BIT = 36
    val irregular = sig == 0

    var k = 0
    if (!irregular) k = (expBin * 1233) >> 12
    else {
      k = (expBin * 1233 - 512) >> 12
      h37p = 37 + expBin + ((k * -1701 + -1701) >> 9)
    }

    val pow10Hi = pow10Reverse(45 + k)
    val cb = sigBin.toLong << h37p
    val hi64 = EiselLemireDouble.unsignedMultiplyHigh(cb, pow10Hi)
    val halfUlp = (pow10Hi >>> (65 - h37p)) + ((sig + 1) & 1)
    val dotOne = hi64 & ((1L << BIT) - 1)
    val mUp = (hi64 + halfUlp) >>> BIT
    var upDown = if (ugt(mUp, (hi64 - halfUlp) >>> BIT)) 1 else 0
    // Pure next digit: xjb bakes an ASCII '0' base into its constant; dropping it leaves just the digit (x64 variant).
    var one = (dotOne * 5 + ((1L << 34) - 7) + (dotOne >>> 32)) >>> 35
    if (irregular) {
      if (expBin == 31 - 150 || expBin == 214 - 150 || expBin == 217 - 150) one += 1
      upDown = if (ugt(mUp, (hi64 - (halfUlp >>> 1)) >>> BIT)) 1 else 0
    }

    // value = shortestSig * 10^k
    var shortestSig = mUp * 10 + (if (upDown != 0) 0L else one)
    var k10 = k
    while (shortestSig % 10 == 0) { shortestSig /= 10; k10 += 1 }

    val nd = XjbDouble.decimalLength(shortestSig)
    val e = k10 + nd - 1
    index += XjbDouble.writeShortest(out, index, shortestSig, nd, e)
    index - offset
  }
}
