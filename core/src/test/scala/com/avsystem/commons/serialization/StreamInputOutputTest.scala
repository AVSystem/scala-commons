package com.avsystem.commons
package serialization

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import org.scalatest.funsuite.AnyFunSuite

case class Wrap(x: Int) derives GenCodec

case class Obj(k1: Int, k2: String) derives GenCodec

case class FieldTypes(
  a: String,
  b: Unit,
  c: String,
  d: Char,
  e: Boolean,
  l1: List[Null],
  f: Boolean,
  g: Byte,
  h: Short,
  i: Int,
  j: Long,
  k: Double,
  l: BigInt,
  m: BigDecimal,
  n: Array[Byte],
  o: Obj,
  p: List[List[Obj]],
) derives GenCodec

class StreamInputOutputTest extends AnyFunSuite {

  val fieldTypesInstance: FieldTypes = FieldTypes(
    null.asInstanceOf[String],
    (),
    "str",
    'c',
    e = false,
    List(null, null),
    f = true,
    -3,
    -4,
    -5,
    -6,
    -7.3,
    BigInt("5345224654563123434325343"),
    BigDecimal(BigInt("2356342454564522135435"), 150),
    Array[Byte](1, 2, 4, 2),
    Obj(10, "x"),
    List(
      List.empty,
      List(Obj(123, "y"), Obj(124, "z")),
    ),
  )

  def outputs(): (ByteArrayOutputStream, StreamOutput) = {
    val os = new ByteArrayOutputStream()
    val output = new StreamOutput(new DataOutputStream(os))
    (os, output)
  }

  def inputs(os: ByteArrayOutputStream): (ByteArrayInputStream, StreamInput) = {
    val is = new ByteArrayInputStream(os.toByteArray)
    val input = new StreamInput(new DataInputStream(is))
    (is, input)
  }

  def encDec[A: GenCodec](a: A): A = {
    val (os, output) = outputs()
    GenCodec.write(output, a)
    val (is, input) = inputs(os)
    val result = GenCodec.read[A](input)
    assert(is.available() == 0)
    result
  }

  def assertEncDec[A: GenCodec](a: A): Unit = assert(a == encDec(a))

  test("simple encode/decode") {
    assertEncDec(1)
//    assert(Array[Byte](1, 3, 8).sameElements(encDec(Array[Byte](1, 3, 8))))
    assertEncDec("x")
    assertEncDec(List.empty[String])
    assertEncDec(List[String]("   "))
    assertEncDec(List[Null](null, null))
    assertEncDec(Wrap(5))
    assertEncDec(Obj(12, "some string"))
  }

  test("encode and decode all field types in a complicated structure") {
    val encoded = encDec(fieldTypesInstance)
    assert(fieldTypesInstance.n.sameElements(encoded.n))
    assert(fieldTypesInstance == encoded.copy(n = fieldTypesInstance.n))
  }

  test("raw API usage") {
    val (os, output) = outputs()

    val objOutput = output.writeObject()
    objOutput.writeField("k1").writeSimple().writeInt(5)
    objOutput.finish()

    val (is, input) = inputs(os)

    val objIn = input.readObject()

    val v = objIn.nextField()
    assert(v.fieldName == "k1")
    assert(v.readSimple().readInt() == 5)

    assert(!objIn.hasNext)
    assert(is.available() == 0)
  }

  test("skipping works") {
    val (os, output) = outputs()
    GenCodec.write(output, fieldTypesInstance)
    val (is, input) = inputs(os)

    val objInput = input.readObject()

    (1 to 2).foreach(_ => objInput.nextField().skip())

    assert(objInput.nextField().readSimple().readString() == "str")

    (4 until fieldTypesInstance.productArity).foreach(_ => objInput.nextField().skip())

    val listInput = objInput.nextField().readList()
    listInput.nextElement().skip()
    val innerList = listInput.nextElement().readList()
    val innerObjInput = innerList.nextElement().readObject()

    assert(innerObjInput.nextField().readSimple().readInt() == 123)
    assert(innerObjInput.nextField().readSimple().readString() == "y")
    assert(!innerObjInput.hasNext)

    innerList.nextElement().skip()

    assert(!innerList.hasNext)
    assert(!listInput.hasNext)
    assert(!objInput.hasNext)

    assert(is.available() == 0)
  }

}
