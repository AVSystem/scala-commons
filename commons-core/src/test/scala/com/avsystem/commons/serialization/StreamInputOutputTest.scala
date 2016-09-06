package com.avsystem.commons
package serialization

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import org.scalatest.FunSuite

case class Wrap(x: Int)

case class Obj(k1: Int, k2: String)

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
  l: Array[Byte],
  m: Obj,
  n: List[List[Obj]]
)

class StreamInputOutputTest extends FunSuite {

  val fieldTypesInstance = FieldTypes(
    null,
    (),
    "str",
    'c',
    e=false,
    List(null, null),
    f=true,
    -3,
    -4,
    -5,
    -6,
    -7.3,
    Array[Byte](1,2,4,2),
    Obj(10, "x"),
    List(
      List.empty,
      List(Obj(123, "y"), Obj(124, "z"))
    )
  )

  implicit val wrapCodec = GenCodec.materialize[Wrap]
  implicit val objCodec = GenCodec.materialize[Obj]
  implicit val fieldTypesCodec = GenCodec.materialize[FieldTypes]

  def outputs() = {
    val os = new ByteArrayOutputStream()
    val output = new StreamOutput(new DataOutputStream(os))
    (os, output)
  }

  def inputs(os: ByteArrayOutputStream) = {
    val is = new ByteArrayInputStream(os.toByteArray)
    val input = new StreamInput(new DataInputStream(is))
    (is, input)
  }

  def encDec[A](a: A)(implicit c: GenCodec[A]): A = {
    val (os, output) = outputs()
    GenCodec.write(output, a)
    val (is, input) = inputs(os)
    val result = GenCodec.read[A](input)
    assert(is.available() == 0)
    result
  }

  def assertEncDec[A](a: A)(implicit c: GenCodec[A]): Unit = assert(a == encDec(a))

  test("simple encode/decode") {
    assertEncDec(1)
    assert(Array[Byte](1,3,8) sameElements  encDec(Array[Byte](1,3,8)))
    assertEncDec("x")
    assertEncDec(List.empty[String])
    assertEncDec(List[String]("   "))
    assertEncDec(List[Null](null, null))
    assertEncDec(Wrap(5))
    assertEncDec(Obj(12, "some string"))
  }

  test("encode and decode all field types in a complicated structure") {
    val encoded = encDec(fieldTypesInstance)
    assert(fieldTypesInstance.l sameElements encoded.l)
    assert(fieldTypesInstance == encoded.copy(l = fieldTypesInstance.l))
  }

  test("raw API usage") {
    val (os, output) = outputs()

    val objOutput = output.writeObject()
    objOutput.writeField("k1").writeInt(5)
    objOutput.finish()

    val (is, input) = inputs(os)

    val objIn = input.readObject().get

    val (k, v) = objIn.nextField()
    assert(k == "k1")
    assert(v.readInt().get == 5)

    assert(!objIn.hasNext)
    assert(is.available() == 0)
  }

  test("skipping works") {
    val (os, output) = outputs()
    GenCodec.write(output, fieldTypesInstance)
    val (is, input) = inputs(os)

    val objInput = input.readObject().get

    (1 to 2).foreach(_ => objInput.nextField()._2.skip())

    assert(objInput.nextField()._2.readString().get == "str")

    (4 until fieldTypesInstance.productArity).foreach(_ => objInput.nextField()._2.skip())

    val listInput = objInput.nextField()._2.readList().get
    listInput.nextElement().skip()
    val innerList = listInput.nextElement().readList().get
    val innerObjInput = innerList.nextElement().readObject().get

    assert(innerObjInput.nextField()._2.readInt().get == 123)
    assert(innerObjInput.nextField()._2.readString().get == "y")
    assert(!innerObjInput.hasNext)

    innerList.nextElement().skip()

    assert(!innerList.hasNext)
    assert(!listInput.hasNext)
    assert(!objInput.hasNext)

    assert(is.available() == 0)
  }

}
