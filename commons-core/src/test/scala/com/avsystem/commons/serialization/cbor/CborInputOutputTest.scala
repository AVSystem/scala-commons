package com.avsystem.commons
package serialization.cbor

import java.io.{ByteArrayOutputStream, DataOutputStream}

import com.avsystem.commons.misc.{Bytes, Timestamp}
import com.avsystem.commons.serialization.{GenCodec, GenCodecRoundtripTest, HasGenCodec, Input, Output, SizePolicy, transientDefault}
import org.scalactic.source.Position
import org.scalatest.FunSuite

case class Record(
  b: Boolean,
  i: Int,
  l: List[String],
  d: Double,
  @transientDefault s: String = ""
)
object Record extends HasGenCodec[Record]

class CborInputOutputTest extends FunSuite {
  private def roundtrip[T: GenCodec](
    value: T,
    binary: String,
    labels: FieldLabels = FieldLabels.NoLabels
  )(implicit pos: Position): Unit =
    test(s"${pos.lineNumber}: $value") {
      val baos = new ByteArrayOutputStream
      val output = new CborOutput(new DataOutputStream(baos), labels, SizePolicy.WhenCheap)
      GenCodec.write[T](output, value)
      val bytes = baos.toByteArray
      assert(Bytes(bytes).toString == binary)
      assert(CborInput.read[T](bytes, labels) == value)
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

  val labels: FieldLabels = new FieldLabels {
    def label(field: String): Opt[Int] = field match {
      case "a" => Opt(-1)
      case "b" => Opt(0)
      case _ => Opt.Empty
    }
    def field(label: Int): Opt[String] = label match {
      case -1 => Opt("a")
      case 0 => Opt("b")
      case _ => Opt.Empty
    }
  }
  roundtrip(Map("a" -> 1, "b" -> 2, "c" -> 3), "A320010002616303", labels)

  roundtrip(
    Record(b = true, 42, List("a", "ajskd", "kek"), 3.14, "fuuuu"),
    "A56162F56169182A616C9F616165616A736B64636B656BFF6164FB40091EB851EB851F6173656675757575"
  )
  roundtrip(
    Record(b = true, 42, List("a", "ajskd", "kek"), 3.14),
    "A46162F56169182A616C9F616165616A736B64636B656BFF6164FB40091EB851EB851F"
  )

  test("chunked text string") {
    assert(CborInput.read[String](Bytes.parse("7F626162626162626162FF").bytes) == "ababab")
  }

  test("chunked byte string") {
    assert(CborInput.read[Bytes](Bytes.parse("5F426162426162426162FF").bytes) == Bytes("ababab"))
  }
}

class CborGenCodecRoundtripTest extends GenCodecRoundtripTest {
  type Raw = RawCbor

  def writeToOutput(write: Output => Unit): RawCbor = {
    val baos = new ByteArrayOutputStream
    write(new CborOutput(new DataOutputStream(baos), FieldLabels.NoLabels, SizePolicy.WhenCheap))
    RawCbor(baos.toByteArray)
  }

  def createInput(raw: RawCbor): Input =
    new CborInput(new CborReader(raw), FieldLabels.NoLabels)
}
