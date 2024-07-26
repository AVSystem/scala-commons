package com.avsystem.commons
package serialization.nativejs

import com.avsystem.commons.misc.Timestamp
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
    rawJson: WrappedJson,
  )
  object TestModel extends HasGenCodec[TestModel]
}

class NativeJsonInputOutputTest extends AnyFunSuite {
  import NativeJsonInputOutputTest._

  test("Bilateral serialization - raw string options") {
    val options = NativeFormatOptions.RawString
    bilateralTyped(testModel, options)
  }

  test("Bilateral serialization - number options") {
    val options = NativeFormatOptions(longFormat = NativeLongFormat.JsNumber, dateFormat = NativeDateFormat.JsNumber)
    bilateralTyped(testModel, options)
  }

  test("Bilateral serialization - typed options") {
    val options = NativeFormatOptions(longFormat = NativeLongFormat.JsBigInt, dateFormat = NativeDateFormat.JsDate)
    bilateralTyped(testModel, options)
  }

  private def testModel: TestModel = TestModel(
    str = "abc",
    int = 123,
    long = 10_000_000_123L,
    time = Timestamp.now(),
    list = Seq(1, 2, 3),
    map = Map("Abc" -> "1", "xyz" -> "10000"),
    rawJson = WrappedJson("""{"a":1,"b":"c"}"""),
  )

  private def bilateralTyped[T: GenCodec](input: T, options: NativeFormatOptions): Unit = {
    val raw = NativeJsonOutput.write(input, options)
    val deserialized = NativeJsonInput.read[T](raw, options)
    assert(deserialized == input)
  }
}
