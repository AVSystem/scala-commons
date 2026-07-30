package com.avsystem.commons
package serialization.nativejs

import com.avsystem.commons.misc.{Bytes, Timestamp}
import com.avsystem.commons.serialization.json.WrappedJson
import com.avsystem.commons.serialization.{flatten, optionalParam, GenCodec, HasGenCodec, ObjectInput}
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

  // @flatten so that the case name is read from the `_case` field via ObjectInput.peekField
  @flatten sealed trait SealedModel
  object SealedModel extends HasGenCodec[SealedModel] {
    case class Num(value: Int) extends SealedModel
    case class Str(value: String) extends SealedModel
  }
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

  // --- `null`, unlike `undefined`, is a value and must not be treated as an absent field ---

  test("peekField treats a null value as a present field") {
    val dict = js.Dictionary[js.Any]("nullField" -> null, "undef" -> js.undefined)
    val objectInput = new NativeJsonInput(dict, NativeFormatOptions.RawString).readObject()
    val peeked = objectInput.peekField("nullField")
    assert(peeked.isDefined) // present-with-null -> present
    assert(peeked.get.fieldName == "nullField")
    assert(peeked.get.readNull()) // and its value is null
    assert(objectInput.peekField("undef").isEmpty) // for contrast
  }

  test("hasNext sees null-valued fields but not undefined ones") {
    def objectInput(dict: js.Dictionary[js.Any]): ObjectInput =
      new NativeJsonInput(dict, NativeFormatOptions.RawString).readObject()

    assert(objectInput(js.Dictionary[js.Any]("a" -> null)).hasNext)
    assert(!objectInput(js.Dictionary[js.Any]("a" -> js.undefined)).hasNext)
  }

  test("nextField yields null-valued fields") {
    val dict = js.Dictionary[js.Any]("a" -> js.undefined, "b" -> null)
    val objectInput = new NativeJsonInput(dict, NativeFormatOptions.RawString).readObject()
    val fieldInput = objectInput.nextField()
    assert(fieldInput.fieldName == "b")
    assert(fieldInput.readNull())
    assert(!objectInput.hasNext)
  }

  test("null entries are preserved when reading a Map (iterator path)") {
    val dict = js.Dictionary[js.Any]("a" -> "1", "b" -> null, "c" -> js.undefined)
    assert(NativeJsonInput.read[Map[String, String]](dict) == Map("a" -> "1", "b" -> null))
  }

  test("null field values are read as empty for optional fields") {
    val dict = js.Dictionary[js.Any]("required" -> "abc", "opt" -> null, "option" -> null)
    assert(NativeJsonInput.read[OptionalFieldsModel](dict) == OptionalFieldsModel("abc", Opt.Empty, None, 42))
  }

  test("null is read as null for a nullable field") {
    val dict = js.Dictionary[js.Any]("required" -> null)
    assert(NativeJsonInput.read[OptionalFieldsModel](dict) == OptionalFieldsModel(null, Opt.Empty, None, 42))
  }

  test("null for a non-nullable field fails instead of falling back to the default value") {
    val dict = js.Dictionary[js.Any]("required" -> "abc", "withDefault" -> null)
    // in contrast to `undefined`, which would make the field absent and thus yield the default
    val failure = intercept[GenCodec.ReadFailure](NativeJsonInput.read[OptionalFieldsModel](dict))
    assert(!failure.isInstanceOf[GenCodec.MissingField])
  }

  test("null case field is not treated as a missing case field") {
    // sanity check of the flat encoding this test relies on
    assert(NativeJsonInput.read[SealedModel](js.Dictionary[js.Any]("_case" -> "Num", "value" -> 5)) == SealedModel.Num(5))

    // null `_case` is a value that fails to be read as a case name, not an absent case field
    val failure = intercept[GenCodec.FieldReadFailed] {
      NativeJsonInput.read[SealedModel](js.Dictionary[js.Any]("_case" -> null, "value" -> 5))
    }
    assert(failure.fieldName == "_case")

    // whereas an undefined `_case` makes the case field absent
    assertThrows[GenCodec.MissingCase] {
      NativeJsonInput.read[SealedModel](js.Dictionary[js.Any]("_case" -> js.undefined, "value" -> 5))
    }
  }

  test("null survives a write-read round trip") {
    testCases.foreach { case BilateralTestCase(_, options, testStringRepr) =>
      val model = OptionalFieldsModel(required = null, opt = Opt.Empty, option = None, withDefault = 7)
      bilateralTyped(model, options)
      if (testStringRepr) bilateralString(model, options)
    }
  }

  test("top-level null is read as a null value") {
    assert(NativeJsonInput.read[String](null) == null)
    assert(NativeJsonInput.read[Opt[String]](null).isEmpty)
    assert(NativeJsonInput.readString[Option[Int]]("null").isEmpty)
  }
}
