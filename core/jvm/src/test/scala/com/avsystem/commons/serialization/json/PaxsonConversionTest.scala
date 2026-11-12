package com.avsystem.commons
package serialization.json

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.math.{MathContext, RoundingMode}

/** Correctness tests built from the "hardest to round" stress vectors in Vern Paxson's report
  * [[https://www.icir.org/vern/papers/testbase-report.pdf "A Program for Testing IEEE Decimal-Binary Conversion"]]
  * (Tables 1-4 for double / 53-bit, Tables 14-17 for float / 24-bit).
  *
  * These are decimal inputs that sit just below / just above exactly half an ULP between two adjacent representable
  * values (requiring up to 60+ bits of extra precision to round correctly), and binary values that are the hardest to
  * format. Paxson's central point is that such adversarial inputs are extremely unlikely to be produced by random
  * testing — so our large random round-trip sweeps in [[JsonStringFastDoubleTest]] / [[JsonStringFastFloatTest]] can
  * pass while a subtly-wrong fast path silently returns a confidently-wrong (non-NaN, so un-caught by the fallback net)
  * result on exactly these values. This suite exercises that gap directly.
  *
  *   - Reading (decimal -> binary): every vector must parse bit-identically to `java.lang.Double`/`Float.parse*` — i.e.
  *     the Eisel-Lemire fast path either rounds correctly or correctly detects ambiguity and falls back.
  *   - Writing (binary -> decimal): every vector must (a) round-trip to the exact same value and (b) be the *shortest*
  *     decimal that does so (verified against an independent BigDecimal oracle, since on this JDK `Double.toString` is
  *     not itself guaranteed shortest).
  */
class PaxsonConversionTest extends AnyFunSuite with Matchers {

  private def dbits(d: Double): Long = java.lang.Double.doubleToLongBits(d)
  private def fbits(f: Float): Int = java.lang.Float.floatToIntBits(f)

  // --- oracles -------------------------------------------------------------------------------------------------------

  /** Minimum number of significant decimal digits whose round-half-even rounding parses back to `value`. This is the
    * definition of the shortest representation, computed independently of the codec under test.
    */
  private def shortestSigDigitsD(value: Double): Int = {
    val exact = new JBigDecimal(value)
    var d = 1
    while (d < 17) {
      val rounded = exact.round(new MathContext(d, RoundingMode.HALF_EVEN))
      if (java.lang.Double.parseDouble(rounded.toString) == value) return d
      d += 1
    }
    17
  }

  private def shortestSigDigitsF(value: Float): Int = {
    val exact = new JBigDecimal(value.toDouble)
    var d = 1
    while (d < 9) {
      val rounded = exact.round(new MathContext(d, RoundingMode.HALF_EVEN))
      if (java.lang.Float.parseFloat(rounded.toString) == value) return d
      d += 1
    }
    9
  }

  private def sigDigits(numericText: String): Int =
    new JBigDecimal(numericText).stripTrailingZeros().precision()

  private def writeD(d: Double): String = {
    val sb = new java.lang.StringBuilder
    XjbDouble.appendTo(sb, d)
    sb.toString
  }
  private def writeF(f: Float): String = {
    val sb = new java.lang.StringBuilder
    XjbFloat.appendTo(sb, f)
    sb.toString
  }

  // --- assertions ----------------------------------------------------------------------------------------------------

  private def assertParsesLikeJdkD(s: String): Unit =
    Seq(s, "-" + s).foreach { str =>
      val expected = java.lang.Double.parseDouble(str)
      assert(
        dbits(EiselLemireDouble.parse(str)) == dbits(expected),
        s"parse '$str' -> ${EiselLemireDouble.parse(str)} (jdk $expected)",
      )
      // also through the full input, exercising readDouble end-to-end
      assert(dbits(JsonStringInput.read[Double](str)) == dbits(expected), s"read '$str'")
    }

  private def assertParsesLikeJdkF(s: String): Unit =
    Seq(s, "-" + s).foreach { str =>
      val expected = java.lang.Float.parseFloat(str)
      assert(
        fbits(EiselLemireFloat.parse(str)) == fbits(expected),
        s"parse '$str' -> ${EiselLemireFloat.parse(str)} (jdk $expected)",
      )
      assert(fbits(JsonStringInput.read[Float](str)) == fbits(expected), s"read '$str'")
    }

  private def assertFormatsD(value: Double): Unit = {
    val text = writeD(value)
    assert(dbits(java.lang.Double.parseDouble(text)) == dbits(value), s"round-trip: $value -> '$text'")
    assert(
      dbits(JsonStringInput.read[Double](JsonStringOutput.write(value))) == dbits(value),
      s"end-to-end: $value",
    )
    if (value != 0.0)
      assert(
        sigDigits(text) == shortestSigDigitsD(value),
        s"shortest: $value -> '$text' uses ${sigDigits(text)} digits, min is ${shortestSigDigitsD(value)}",
      )
  }

  private def assertFormatsF(value: Float): Unit = {
    val text = writeF(value)
    assert(fbits(java.lang.Float.parseFloat(text)) == fbits(value), s"round-trip: $value -> '$text'")
    assert(
      fbits(JsonStringInput.read[Float](JsonStringOutput.write(value))) == fbits(value),
      s"end-to-end: $value",
    )
    if (value != 0.0f)
      assert(
        sigDigits(text) == shortestSigDigitsF(value),
        s"shortest: $value -> '$text' uses ${sigDigits(text)} digits, min is ${shortestSigDigitsF(value)}",
      )
  }

  // --- vectors -------------------------------------------------------------------------------------------------------

  // Decimal parse vectors: significand digits * 10^exp. `sig` kept as a String to preserve exact digits.
  private def dec(sig: String, exp: Int): String = s"${sig}e$exp"

  // Table 1: hardest decimal->53-bit-binary inputs, excess just below 1/2 ULP.
  private val doubleParseBelow: List[String] = List(
    dec("5", 125),
    dec("69", 267),
    dec("999", -26),
    dec("7861", -34),
    dec("75569", -254),
    dec("928609", -261),
    dec("9210917", 80),
    dec("84863171", 114),
    dec("653777767", 273),
    dec("5232604057", -298),
    dec("27235667517", -109),
    dec("653532977297", -123),
    dec("3142213164987", -294),
    dec("46202199371337", -72),
    dec("231010996856685", -73),
    dec("9324754620109615", 212),
    dec("78459735791271921", 49),
    dec("272104041512242479", 200),
    dec("6802601037806061975", 198),
    dec("20505426358836677347", -221),
    dec("836168422905420598437", -234),
    dec("4891559871276714924261", 222),
  )

  // Table 2: hardest decimal->53-bit-binary inputs, excess just above 1/2 ULP.
  private val doubleParseAbove: List[String] = List(
    dec("9", -265),
    dec("85", -37),
    dec("623", 100),
    dec("3571", 263),
    dec("81661", 153),
    dec("920657", -23),
    dec("4603285", -24),
    dec("87575437", -309),
    dec("245540327", 122),
    dec("6138508175", 120),
    dec("83356057653", 193),
    dec("619534293513", 124),
    dec("2335141086879", 218),
    dec("36167929443327", -159),
    dec("609610927149051", -255),
    dec("3743626360493413", -165),
    dec("94080055902682397", -242),
    dec("899810892172646163", 283),
    dec("7120190517612959703", 120),
    dec("25188282901709339043", -252),
    dec("308984926168550152811", -52),
    dec("6372891218502368041059", 64),
  )

  // Table 14/15: hardest decimal->24-bit-binary (float) inputs, below / above 1/2 ULP.
  private val floatParseBelow: List[String] = List(
    dec("5", -20),
    dec("67", 14),
    dec("985", 15),
    dec("7693", -42),
    dec("55895", -16),
    dec("996622", -44),
    dec("7038531", -32),
    dec("60419369", -46),
    dec("702990899", -20),
    dec("6930161142", -48),
    dec("25933168707", 13),
    dec("596428896559", 20),
  )
  private val floatParseAbove: List[String] = List(
    dec("3", -23),
    dec("57", 18),
    dec("789", -35),
    dec("2539", -18),
    dec("76173", 28),
    dec("887745", -11),
    dec("5382571", -37),
    dec("82381273", -35),
    dec("750486563", -38),
    dec("3752432815", -39),
    dec("75224575729", -45),
    dec("459926601011", 15),
  )

  // Binary format vectors: significand * 2^exp (Tables 3/4 double, 16/17 float). Math.scalb is exact here since the
  // significand is < 2^53 (resp. 2^24) and the result is in the normal range.
  private def scalbD(sig: Long, exp: Int): Double = Math.scalb(sig.toDouble, exp)
  private def scalbF(sig: Int, exp: Int): Float = Math.scalb(sig.toFloat, exp)

  // Table 3: hardest 53-bit-binary->decimal, below 1/2 ULP.
  private val doubleFormatBelow: List[Double] = List(
    scalbD(8511030020275656L, -342),
    scalbD(5201988407066741L, -824),
    scalbD(6406892948269899L, 237),
    scalbD(8431154198732492L, 72),
    scalbD(6475049196144587L, 99),
    scalbD(8274307542972842L, 726),
    scalbD(5381065484265332L, -456),
    scalbD(6761728585499734L, -1057),
    scalbD(7976538478610756L, 376),
    scalbD(5982403858958067L, 377),
    scalbD(5536995190630837L, 93),
    scalbD(7225450889282194L, 710),
    scalbD(7225450889282194L, 709),
    scalbD(8703372741147379L, 117),
    scalbD(8944262675275217L, -1001),
    scalbD(7459803696087692L, -707),
    scalbD(6080469016670379L, -381),
    scalbD(8385515147034757L, 721),
    scalbD(7514216811389786L, -828),
    scalbD(8397297803260511L, -345),
    scalbD(6733459239310543L, 202),
    scalbD(8091450587292794L, -473),
  )

  // Table 4: hardest 53-bit-binary->decimal, above 1/2 ULP.
  private val doubleFormatAbove: List[Double] = List(
    scalbD(6567258882077402L, 952),
    scalbD(6712731423444934L, 535),
    scalbD(6712731423444934L, 534),
    scalbD(5298405411573037L, -957),
    scalbD(5137311167659507L, -144),
    scalbD(6722280709661868L, 363),
    scalbD(5344436398034927L, -169),
    scalbD(8369123604277281L, -853),
    scalbD(8995822108487663L, -780),
    scalbD(8942832835564782L, -383),
    scalbD(8942832835564782L, -384),
    scalbD(8942832835564782L, -385),
    scalbD(6965949469487146L, -249),
    scalbD(6965949469487146L, -250),
    scalbD(6965949469487146L, -251),
    scalbD(7487252720986826L, 548),
    scalbD(5592117679628511L, 164),
    scalbD(8887055249355788L, 665),
    scalbD(6994187472632449L, 690),
    scalbD(8797576579012143L, 588),
    scalbD(7363326733505337L, 272),
    scalbD(8549497411294502L, -448),
  )

  // Table 16/17: hardest 24-bit-binary->decimal (float), below / above 1/2 ULP.
  private val floatFormatBelow: List[Float] = List(
    scalbF(12676506, -102),
    scalbF(12676506, -103),
    scalbF(15445013, 86),
    scalbF(13734123, -138),
    scalbF(12428269, -130),
    scalbF(15334037, -146),
    scalbF(11518287, -41),
    scalbF(12584953, -145),
    scalbF(15961084, -125),
    scalbF(14915817, -146),
    scalbF(10845484, -102),
    scalbF(16431059, -61),
  )
  private val floatFormatAbove: List[Float] = List(
    scalbF(16093626, 69),
    scalbF(9983778, 25),
    scalbF(12745034, 104),
    scalbF(12706553, 72),
    scalbF(11005028, 45),
    scalbF(15059547, 71),
    scalbF(16015691, -99),
    scalbF(8667859, 56),
    scalbF(14855922, -82),
    scalbF(14855922, -83),
    scalbF(10144164, -110),
    scalbF(13248074, 95),
  )

  // Named error cases the paper reports real systems getting wrong (Tables 12/13, and the monotonicity examples).
  private val doubleNamedParse: List[String] =
    List("1e+126", "9.51206426453718e-27", "3e+97", "9e+26", "5.5225015152609010e+14", "5.5225015152609011e+14")
  private val floatNamedParse: List[String] =
    List("7.038531e-26", "4.1358803e34", "9.55610858e-6", "9.55610857e-6")

  // --- tests ---------------------------------------------------------------------------------------------------------

  test("double parse: Paxson Table 1 (just below 1/2 ULP) matches JDK bit-for-bit") {
    doubleParseBelow.foreach(assertParsesLikeJdkD)
  }
  test("double parse: Paxson Table 2 (just above 1/2 ULP) matches JDK bit-for-bit") {
    doubleParseAbove.foreach(assertParsesLikeJdkD)
  }
  test("double parse: named hard cases from the paper's error tables") {
    doubleNamedParse.foreach(assertParsesLikeJdkD)
  }
  test("float parse: Paxson Table 14 (just below 1/2 ULP) matches JDK bit-for-bit") {
    floatParseBelow.foreach(assertParsesLikeJdkF)
  }
  test("float parse: Paxson Table 15 (just above 1/2 ULP) matches JDK bit-for-bit") {
    floatParseAbove.foreach(assertParsesLikeJdkF)
  }
  test("float parse: named hard cases from the paper") {
    floatNamedParse.foreach(assertParsesLikeJdkF)
  }

  test("double format: Paxson Table 3 (below 1/2 ULP) round-trips and is shortest") {
    doubleFormatBelow.foreach { v => assertFormatsD(v); assertFormatsD(-v) }
  }
  test("double format: Paxson Table 4 (above 1/2 ULP) round-trips and is shortest") {
    doubleFormatAbove.foreach { v => assertFormatsD(v); assertFormatsD(-v) }
  }
  test("float format: Paxson Table 16 (below 1/2 ULP) round-trips and is shortest") {
    floatFormatBelow.foreach { v => assertFormatsF(v); assertFormatsF(-v) }
  }
  test("float format: Paxson Table 17 (above 1/2 ULP) round-trips and is shortest") {
    floatFormatAbove.foreach { v => assertFormatsF(v); assertFormatsF(-v) }
  }

  test("boundary/subnormal doubles round-trip and parse correctly") {
    val vs = List(
      java.lang.Double.MIN_VALUE,
      2.0 * java.lang.Double.MIN_VALUE,
      java.lang.Double.MIN_NORMAL,
      java.lang.Double.MAX_VALUE,
      1.0,
      0.5,
      4.9e-324,
      2.2250738585072014e-308,
    )
    vs.foreach { v => assertFormatsD(v); assertParsesLikeJdkD(v.toString) }
  }
  test("boundary/subnormal floats round-trip and parse correctly") {
    val vs = List(
      java.lang.Float.MIN_VALUE,
      2.0f * java.lang.Float.MIN_VALUE,
      java.lang.Float.MIN_NORMAL,
      java.lang.Float.MAX_VALUE,
      1.0f,
      0.5f,
      1.4e-45f,
      1.17549435e-38f,
    )
    vs.foreach { v => assertFormatsF(v); assertParsesLikeJdkF(v.toString) }
  }
}
