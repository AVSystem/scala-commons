package com.avsystem.commons
package serialization.nativejs

import com.avsystem.commons.misc.{Bytes, Timestamp}
import com.avsystem.commons.serialization.json.WrappedJson
import com.avsystem.commons.serialization.{GenCodec, HasGenCodec}
import org.scalatest.funsuite.AnyFunSuite

object NativeJsonInputOutputTest {

  case class TestModel(
    str: String,
    int: Int,
    long: Long,
    time: Timestamp,
    list: Seq[Int],
    map: Map[String, String],
    binary: Bytes,
    bigInt: BigInt,
    rawJson: WrappedJson,
  )
  object TestModel extends HasGenCodec[TestModel]
}

class NativeJsonInputOutputTest extends AnyFunSuite {
  import NativeJsonInputOutputTest._

  case class BilateralTestCase(name: String, options: NativeFormatOptions, testStringRepr: Boolean = true)

  private val testCases = Seq(
    BilateralTestCase("raw string options", NativeFormatOptions.RawString),
    BilateralTestCase(
      "number options",
      NativeFormatOptions(
        longFormat = NativeLongFormat.JsNumber,
        dateFormat = NativeDateFormat.JsNumber),
    ),
    BilateralTestCase(
      "typed options",
      NativeFormatOptions(
        longFormat = NativeLongFormat.JsBigInt,
        NativeDateFormat.JsDate,
        bigIntFormat = NativeBigIntFormat.JsBigInt,
      ),
      testStringRepr = false, // scala.scalajs.js.JavaScriptException: TypeError: Do not know how to serialize a BigInt
    ),
  )

  testCases.foreach { case BilateralTestCase(name, options, testStringRepr) =>
    test(s"Bilateral serialization - $name") {
      bilateralTyped(testModel, options)
    }

    if (testStringRepr) {
      test(s"Bilateral serialization to string - $name") {
        bilateralString(testModel, options)
      }
    }
  }

  private def testModel: TestModel = TestModel(
    str = "abc",
    int = 123,
    long = 10_000_000_123L,
    time = Timestamp.now(),
    list = Seq(1, 2, 3),
    map = Map("Abc" -> "1", "xyz" -> "10000"),
    binary = new Bytes(Array(1, 2, 0, 5)),
    bigInt = BigInt("10000000000000000000"),
    rawJson = WrappedJson("""{"a":1,"b":"c"}"""),
  )

  private def bilateralTyped[T: GenCodec](input: T, options: NativeFormatOptions): Unit = {
    val raw = NativeJsonOutput.write(input, options)
    val deserialized = NativeJsonInput.read[T](raw, options)
    assert(deserialized == input)
  }

  private def bilateralString[T: GenCodec](input: T, options: NativeFormatOptions): Unit = {
    val raw = NativeJsonOutput.writeAsString(input, options)
    val deserialized = NativeJsonInput.readString[T](raw, options)
    assert(deserialized == input)
  }
}
