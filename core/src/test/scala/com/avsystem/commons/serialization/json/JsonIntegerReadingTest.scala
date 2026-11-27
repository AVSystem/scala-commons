package com.avsystem.commons
package serialization.json

import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization.{GenCodec, HasGenCodec}
import org.scalatest.funsuite.AnyFunSuite

object JsonIntegerReadingTest {
  case class IntAndLong(i: Int, l: Long)
  object IntAndLong extends HasGenCodec[IntAndLong]
}

class JsonIntegerReadingTest extends AnyFunSuite {

  import JsonIntegerReadingTest.IntAndLong
  import JsonStringInput.{read, ParseException}

  private val exact = JsonOptions(integerReading = JsonIntegerReading.Exact)

  test("exact reading accepts whole numbers written with a fraction or exponent") {
    assert(read[Byte]("-128.0", exact) == -128.toByte)
    assert(read[Byte]("127e0", exact) == 127.toByte)
    assert(read[Short]("32767.0", exact) == 32767.toShort)
    assert(read[Short]("-32768e0", exact) == -32768.toShort)
    assert(read[Int]("2.0", exact) == 2)
    assert(read[Int]("5e3", exact) == 5000)
    assert(read[Int]("-0.0", exact) == 0)
    assert(read[Int]("2147483647.0", exact) == Int.MaxValue)
    assert(read[Int]("-2147483648.0", exact) == Int.MinValue)
    assert(read[Int]("0e999999999", exact) == 0)
    assert(read[Long]("9007199254740993.0", exact) == 9007199254740993L)
    assert(read[Long]("-9223372036854775808.0", exact) == Long.MinValue)
  }

  test("exact reading rejects non-whole and out-of-range numbers") {
    List("128.0", "-129.0", "1.5").foreach(s => intercept[ParseException](read[Byte](s, exact)))
    intercept[ParseException](read[Short]("32768.0", exact))
    List(
      "2.0000000000000001",
      "2147483647.0000000001",
      "2147483648.0",
      "0.5",
      "1e999999999",
      "1e-999999999",
    ).foreach(s => intercept[ParseException](read[Int](s, exact)))
    List("9007199254740993.00000000000000000001", "9223372036854775808.0", "1e999999999").foreach(s =>
      intercept[ParseException](read[Long](s, exact))
    )
  }

  test("exact reading leaves plain digits unchanged") {
    assert(read[Int]("123", exact) == 123)
    assert(read[Int]("-0", exact) == 0)
    intercept[ParseException](read[Int]("2147483648", exact))
  }

  test("exact reading of numbers encoded as JSON strings") {
    assert(read[Int]("\"1e3\"", exact) == 1000)
    assert(read[Int]("\"+5\"", exact) == 5)
    List("\" 1\"", "\"0x10\"", "\"1d\"", "\"Infinity\"").foreach(s => intercept[ParseException](read[Int](s, exact)))
  }

  test("rounding reading (the default) keeps its current behaviour") {
    assert(read[Int]("2.0000000000000001") == 2)
    assert(read[Int]("2147483647.0000000001") == Int.MaxValue)
    assert(read[Int]("\"1d\"") == 1)
    assert(read[Long]("9007199254740993.00000000000000000001") == 9007199254740993L)
    intercept[ParseException](read[Int]("0.5"))
    assert(read[Byte]("1.0000000000000001") == 1)
    intercept[ParseException](read[Short]("2.5"))
  }

  test("exact reading through a generated codec") {
    assert(read[IntAndLong]("""{"i": 2.0, "l": 5e3}""", exact) == IntAndLong(2, 5000))
    intercept[ReadFailure](read[IntAndLong]("""{"i": 2.5, "l": 1}""", exact))
  }

  test("exact reading of epoch millis timestamps") {
    val options = JsonOptions(dateFormat = JsonDateFormat.EpochMillis, integerReading = JsonIntegerReading.Exact)
    assert(read[java.util.Date]("1.5e12", options) == new java.util.Date(1500000000000L))
    intercept[ReadFailure](read[java.util.Date]("1.5000000000001e12", options))
  }
}
