package com.avsystem.commons
package serialization.cbor

import com.avsystem.commons.misc.{Bytes, Timestamp}
import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization._
import com.avsystem.commons.serialization.json.JsonStringOutput
import org.scalactic.source.Position
import org.scalatest.funsuite.AnyFunSuite

import java.io.{ByteArrayOutputStream, DataOutputStream}

final case class Record(
  b: Boolean,
  i: Int,
  l: List[String],
  d: Double,
  @transientDefault s: String = ""
)
object Record extends HasGenCodec[Record]

final case class CustomKeysRecord(
  @cborKey(1) first: Int,
  @cborKey(true) second: Boolean,
  @cborKey(Vector(1, 2, 3)) third: String,
  strMap: Map[String, Int],
  intMap: Map[Int, String],
)
object CustomKeysRecord extends HasCborCodec[CustomKeysRecord]

final case class CustomKeysRecordWithDefaults(
  @transientDefault @cborKey(1) first: Int = 0,
  @cborKey(true) second: Boolean,
)
object CustomKeysRecordWithDefaults extends HasCborCodec[CustomKeysRecordWithDefaults]

final case class CustomKeysRecordWithNoDefaults(
  @cborKey(1) first: Int = 0,
  @cborKey(true) second: Boolean,
)
object CustomKeysRecordWithNoDefaults extends HasCborCodec[CustomKeysRecordWithNoDefaults]

@cborDiscriminator(0)
sealed trait GenericSealedTrait[+T]
object GenericSealedTrait extends HasPolyCborCodec[GenericSealedTrait] {
  @cborKey(0)
  case class Success[+T](@cborKey(1) value: T) extends GenericSealedTrait[T]
  @cborKey(1)
  case class Failure(@cborKey(1) message: String) extends GenericSealedTrait[Nothing]
}

@cborDiscriminator(0)
sealed trait CustomKeysFlatUnion extends Product with Serializable
object CustomKeysFlatUnion extends HasCborCodec[CustomKeysFlatUnion] {
  @cborKey(1) case class IntCase(@cborKey(1) int: Int) extends CustomKeysFlatUnion
  @cborKey(2) case class StrCase(@cborKey(1) str: String) extends CustomKeysFlatUnion
  @cborKey(3) case object EmptyCase extends CustomKeysFlatUnion
  case class BoolCase(bool: Boolean) extends CustomKeysFlatUnion
}

sealed trait CustomKeysNestedUnion extends Product with Serializable
object CustomKeysNestedUnion extends HasCborCodec[CustomKeysNestedUnion] {
  @cborKey(1) case class IntCase(@cborKey(1) int: Int) extends CustomKeysNestedUnion
  @cborKey(2) case class StrCase(@cborKey(1) str: String) extends CustomKeysNestedUnion
  @cborKey(3) case object EmptyCase extends CustomKeysNestedUnion
  case class BoolCase(bool: Boolean) extends CustomKeysNestedUnion
}

class CborInputOutputTest extends AnyFunSuite {
  private def roundtrip[T: GenCodec](
    value: T,
    binary: String,
    keyCodec: CborKeyCodec = CborKeyCodec.Default
  )(implicit pos: Position): Unit =
    test(s"${pos.lineNumber}: $value") {
      assertRoundtrip(value, binary, keyCodec)
    }

  private def assertRoundtrip[T: GenCodec](
    value: T,
    binary: String,
    keyCodec: CborKeyCodec = CborKeyCodec.Default
  )(implicit pos: Position): Unit = {
    val baos = new ByteArrayOutputStream
    val output = new CborOutput(new DataOutputStream(baos), keyCodec, SizePolicy.Optional)
    GenCodec.write[T](output, value)
    val bytes = baos.toByteArray
    assert(Bytes(bytes).toString == binary)
    assert(RawCbor(bytes).readAs[T](keyCodec) == value)
  }

  // binary representation from cbor.me

  roundtrip(null, "F6")
  roundtrip(true, "F5")
  roundtrip(false, "F4")

  roundtrip(Byte.MinValue, "387F")
  roundtrip(-1.toByte, "20")
  roundtrip(0.toByte, "00")
  roundtrip(1.toByte, "01")
  roundtrip(Byte.MaxValue, "187F")

  roundtrip(Short.MinValue, "397FFF")
  roundtrip(-1.toShort, "20")
  roundtrip(0.toShort, "00")
  roundtrip(1.toShort, "01")
  roundtrip(Short.MaxValue, "197FFF")

  roundtrip(Int.MinValue, "3A7FFFFFFF")
  roundtrip(-1, "20")
  roundtrip(0, "00")
  roundtrip(1, "01")
  roundtrip(Int.MaxValue, "1A7FFFFFFF")

  roundtrip(Long.MinValue, "3B7FFFFFFFFFFFFFFF")
  roundtrip(-1L, "20")
  roundtrip(0L, "00")
  roundtrip(1L, "01")
  roundtrip(Long.MaxValue, "1B7FFFFFFFFFFFFFFF")

  roundtrip(Float.NegativeInfinity, "F9FC00")
  roundtrip(Float.MinValue, "FAFF7FFFFF")
  roundtrip(0.0f, "00")
  roundtrip(Float.MinPositiveValue, "FA00000001")
  roundtrip(2.5f, "F94100")
  roundtrip(2.7f, "FA402CCCCD")
  roundtrip(3.14f, "FA4048F5C3")
  roundtrip(1e20f, "FA60AD78EC")
  roundtrip(1e-20f, "FA1E3CE508")
  roundtrip(1e-40f, "FA000116C2")
  roundtrip(Float.MaxValue, "FA7F7FFFFF")
  roundtrip(Float.PositiveInfinity, "F97C00")

  roundtrip(Double.NegativeInfinity, "F9FC00")
  roundtrip(Double.MinValue, "FBFFEFFFFFFFFFFFFF")
  roundtrip(0.0, "00")
  roundtrip(Double.MinPositiveValue, "FB0000000000000001")
  roundtrip(2.5, "F94100")
  roundtrip(2.7f.toDouble, "FA402CCCCD")
  roundtrip(2.7, "FB400599999999999A")
  roundtrip(3.14, "FB40091EB851EB851F")
  roundtrip(1e150, "FB5F138D352E5096AF")
  roundtrip(1e-200, "FB16687E92154EF7AC")
  roundtrip(Double.MaxValue, "FB7FEFFFFFFFFFFFFF")
  roundtrip(Double.PositiveInfinity, "F97C00")

  roundtrip(BigInt("123243534645675672342"), "C24906AE58FDC9BB0BC316")
  roundtrip(BigInt("-123243534645675672342"), "C34906AE58FDC9BB0BC315")
  roundtrip(BigInt("18446744073709551615"), "1BFFFFFFFFFFFFFFFF")
  roundtrip(BigInt("-18446744073709551616"), "3BFFFFFFFFFFFFFFFF")

  roundtrip(BigDecimal("123228383982485398493e38493485"), "C4821A024B5D2DC24906AE232A57117A63DD")

  roundtrip(Timestamp.parse("2019-01-09T14:31:22.000Z"), "C11A5C3605BA")
  roundtrip(Timestamp.parse("2019-01-09T14:31:22.001Z"), "C1FB41D70D816E801062")
  roundtrip(Timestamp.parse("2019-01-09T14:31:22.500Z"), "C1FB41D70D816EA00000")
  roundtrip(Timestamp.parse("2019-01-09T14:31:22.731Z"), "C1FB41D70D816EAEC8B4")

  roundtrip("", "60")
  roundtrip("ab", "626162")
  roundtrip("ąć", "64C485C487")
  roundtrip("a" * 30, "781E616161616161616161616161616161616161616161616161616161616161")

  roundtrip(Bytes(""), "40")
  roundtrip(Bytes("ab"), "426162")
  roundtrip(Bytes("ąć"), "44C485C487")
  roundtrip(Bytes("a" * 30), "581E616161616161616161616161616161616161616161616161616161616161")

  roundtrip(("foo", 42, 3.14), "8363666F6F182AFB40091EB851EB851F")

  roundtrip(List(1, 2, 3), "9F010203FF")
  roundtrip(Vector(1, 2, 3), "83010203")

  roundtrip(IListMap("a" -> 1, "b" -> 2, "c" -> 3), "BF616101616202616303FF")
  roundtrip(Map("a" -> 1, "b" -> 2, "c" -> 3), "A3616101616202616303")

  val keyCodec: CborKeyCodec = new CborKeyCodec {
    def writeFieldKey(fieldName: String, output: CborOutput): Unit = fieldName match {
      case "a" => output.writeInt(-1)
      case "b" => output.writeInt(0)
      case n => output.writeString(n)
    }
    def readFieldKey(input: CborInput): String = input.readInitialByte().majorType match {
      case MajorType.Unsigned | MajorType.Negative => input.readInt() match {
        case -1 => "a"
        case 0 => "b"
        case n => throw new ReadFailure(s"unknown CBOR field label: $n")
      }
      case _ =>
        input.readString()
    }
  }
  roundtrip(Map("a" -> 1, "b" -> 2, "c" -> 3), "A320010002616303", keyCodec)

  roundtrip(
    Record(b = true, 42, List("a", "ajskd", "kek"), 3.14, "fuuuu"),
    "A56162F56169182A616C9F616165616A736B64636B656BFF6164FB40091EB851EB851F6173656675757575"
  )

  roundtrip(
    Record(b = true, 42, List("a", "ajskd", "kek"), 3.14),
    "A46162F56169182A616C9F616165616A736B64636B656BFF6164FB40091EB851EB851F"
  )

  roundtrip(
    CustomKeysRecord(42, second = false, "foo", Map("bar" -> 0), Map(0 -> "bar")),
    "A501182AF5F48301020363666F6F667374724D6170A1636261720066696E744D6170A10063626172"
  )

  roundtrip(
    List(
      CustomKeysFlatUnion.IntCase(42),
      CustomKeysFlatUnion.StrCase("foo"),
      CustomKeysFlatUnion.BoolCase(bool = true),
      CustomKeysFlatUnion.EmptyCase
    ),
    "9FA2000101182AA200020163666F6FA20068426F6F6C4361736564626F6F6CF5A10003FF"
  )

  roundtrip(
    List(
      CustomKeysNestedUnion.IntCase(42),
      CustomKeysNestedUnion.StrCase("foo"),
      CustomKeysNestedUnion.BoolCase(bool = true),
      CustomKeysNestedUnion.EmptyCase
    ),
    "9FA101A101182AA102A10163666F6FA168426F6F6C43617365A164626F6F6CF5A103A0FF"
  )

  test("writing with CBOR optimized codec to non-CBOR output") {
    assert(JsonStringOutput.write(CustomKeysRecord(42, second = true, "foo", Map("foo" -> 1), Map(1 -> "foo"))) ==
      """{"first":42,"second":true,"third":"foo","strMap":{"foo":1},"intMap":{"1":"foo"}}""")
  }

  test("writing with IgnoreTransientDefaultMarker to CBOR output") {
    val baos = new ByteArrayOutputStream
    val output = CustomMarkersOutputWrapper(
      new CborOutput(new DataOutputStream(baos), keyCodec, SizePolicy.Optional),
      IgnoreTransientDefaultMarker,
    )
    val value = CustomKeysRecordWithDefaults(first = 0, second = true)
    GenCodec.write(output, value)
    val bytes = Bytes(baos.toByteArray)

    val expectedRawValue = "A20100F5F5"
    assert(bytes.toString == expectedRawValue)
    assert(RawCbor(bytes.bytes).readAs[CustomKeysRecordWithDefaults](keyCodec) == value)

    // should be the same as model with @transientDefault and serialization ignoring it
    assertRoundtrip(CustomKeysRecordWithNoDefaults(first = 0, second = true), expectedRawValue)
  }

  test("chunked text string") {
    assert(CborInput.readRawCbor[String](RawCbor.fromHex("7F626162626162626162FF")) == "ababab")
  }

  test("chunked byte string") {
    assert(CborInput.readRawCbor[Bytes](RawCbor.fromHex("5F426162426162426162FF")) == Bytes("ababab"))
  }

  roundtrip[GenericSealedTrait[Int]](GenericSealedTrait.Success[Int](234), "A200000118EA")
  roundtrip[GenericSealedTrait[Boolean]](GenericSealedTrait.Failure("error"), "A2000101656572726F72")
}

class CborGenCodecRoundtripTest extends GenCodecRoundtripTest {
  type Raw = RawCbor

  def writeToOutput(write: Output => Unit): RawCbor = {
    val baos = new ByteArrayOutputStream
    write(new CborOutput(new DataOutputStream(baos), CborKeyCodec.Default, SizePolicy.Optional))
    RawCbor(baos.toByteArray)
  }

  def createInput(raw: RawCbor): Input =
    new CborInput(new CborReader(raw), CborKeyCodec.Default)
}
