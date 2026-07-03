package com.avsystem.commons
package serialization.nativejs

import com.avsystem.commons.misc.{Bytes, Timestamp}
import com.avsystem.commons.serialization.json.WrappedJson
import com.avsystem.commons.serialization.{GenCodec, HasGenCodec, optionalParam}
import org.scalatest.funsuite.AnyFunSuite

import scala.scalajs.js

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

  case class OptionalFieldsModel(
    required: String,
    @optionalParam opt: Opt[String],
    @optionalParam option: Option[Int],
    withDefault: Int = 42,
  )
  object OptionalFieldsModel extends HasGenCodec[OptionalFieldsModel]
}

class NativeJsonInputOutputTest extends AnyFunSuite {
  import NativeJsonInputOutputTest._

  case class BilateralTestCase(name: String, options: NativeFormatOptions, testStringRepr: Boolean = true)

  private val testCases = Seq(
    BilateralTestCase("raw string options", NativeFormatOptions.RawString),
    BilateralTestCase(
      "number options",
      NativeFormatOptions(longFormat = NativeLongFormat.JsNumber, dateFormat = NativeDateFormat.JsNumber),
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

  // --- issue #848: JS `undefined` field values must be treated as absent ---

  test("undefined fields are treated as absent when reading a case class") {
    val dict = js.Dictionary[js.Any](
      "required" -> "abc",
      "opt" -> js.undefined,
      "option" -> js.undefined,
      "withDefault" -> js.undefined,
    )
    assert(NativeJsonInput.read[OptionalFieldsModel](dict) == OptionalFieldsModel("abc", Opt.Empty, None, 42))
  }

  test("undefined fields behave identically to omitted fields") {
    val withUndefined = js.Dictionary[js.Any](
      "required" -> "abc",
      "opt" -> js.undefined,
      "option" -> js.undefined,
      "withDefault" -> js.undefined,
    )
    val omitted = js.Dictionary[js.Any]("required" -> "abc")
    assert(NativeJsonInput.read[OptionalFieldsModel](withUndefined) == NativeJsonInput.read[OptionalFieldsModel](omitted))
  }

  test("undefined value for a required field is treated as missing") {
    val dict = js.Dictionary[js.Any]("required" -> js.undefined, "option" -> 5)
    // MissingField (not a generic "cannot read" failure) proves the undefined field was skipped entirely
    assertThrows[GenCodec.MissingField] {
      NativeJsonInput.read[OptionalFieldsModel](dict)
    }
  }

  test("undefined entries are skipped when reading a Map (iterator path)") {
    val dict = js.Dictionary[js.Any]("a" -> "1", "b" -> js.undefined, "c" -> "3")
    assert(NativeJsonInput.read[Map[String, String]](dict) == Map("a" -> "1", "c" -> "3"))
  }

  test("peekField treats an undefined value as an absent field") {
    val dict = js.Dictionary[js.Any]("defined" -> "value", "undef" -> js.undefined)
    val objectInput = new NativeJsonInput(dict, NativeFormatOptions.RawString).readObject()
    assert(objectInput.peekField("undef").isEmpty) // present-but-undefined -> absent
    assert(objectInput.peekField("missing").isEmpty) // truly absent
    assert(objectInput.peekField("defined").isDefined)
  }
}
