package com.avsystem.commons
package serialization.json

import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization.{GenCodec, Input, Output}
import org.scalatest.{FunSuite, Matchers}

import scala.collection.mutable.ListBuffer
import scala.util.Random

class JsonStringInputOutputTest extends FunSuite with SerializationTestUtils with Matchers {

  import JsonStringInput.read
  import JsonStringOutput.write

  test("raw json list test") {
    val jsons = List("123", "null", "\"str\"", "4.5", "[1,2,3]", "{\"a\": 123, \"b\": 3.14}")
    val sb = new JStringBuilder
    val output = new JsonStringOutput(sb)
    val lo = output.writeList()
    jsons.foreach(json => lo.writeElement().writeRawJson(json))
    lo.finish()
    val jsonList = sb.toString

    assert(jsonList == jsons.mkString("[", ",", "]"))

    val input = new JsonStringInput(new JsonReader(jsonList))
    val li = input.readList()
    val resBuilder = new ListBuffer[String]
    while (li.hasNext) {
      resBuilder += li.nextElement().readRawJson()
    }
    assert(resBuilder.result() == jsons)
  }

  test("raw json object test") {
    val jsons = IListMap("a" -> "123", "b" -> "null", "c" -> "\"str\"", "d" -> "4.5", "e" -> "[1,2,3]", "f" -> "{\"a\": 123, \"b\": 3.14}")
    val sb = new JStringBuilder
    val output = new JsonStringOutput(sb)
    val oo = output.writeObject()
    jsons.foreach { case (fn, fv) => oo.writeField(fn).writeRawJson(fv) }
    oo.finish()
    val jsonList = sb.toString

    assert(jsonList == jsons.map({ case (k, v) => s""""$k":$v""" }).mkString("{", ",", "}"))

    val input = new JsonStringInput(new JsonReader(jsonList))
    val oi = input.readObject()
    val resBuilder = IListMap.newBuilder[String, String]
    while (oi.hasNext) {
      val fi = oi.nextField()
      resBuilder += ((fi.fieldName, fi.readRawJson()))
    }
    assert(resBuilder.result() == jsons)
  }

  def roundtrip[T: GenCodec](name: String)(values: Seq[T]): Unit = {
    test(name) {
      val serialized = values.map(write[T])
      val deserialized = serialized.map(read[T])
      deserialized shouldBe values
    }
  }

  roundtrip("integers")(Seq(Int.MinValue, -42, 0, 42, Int.MaxValue))

  roundtrip("longs")(Seq(Int.MaxValue.toLong + 1, Long.MinValue, -783868188, 0, 783868188, Long.MaxValue))

  roundtrip("doubles")(Seq(
    Double.MinPositiveValue, Double.MinValue, -1.750470182E9, Double.MaxValue, Double.PositiveInfinity, Double.NegativeInfinity)
  )
  roundtrip("booleans")(Seq(false, true))

  roundtrip("strings")(Seq("", "a።bc\u0676ąቢść➔Ĳ"))

  roundtrip("simple case classes")(Seq(TestCC(5, 123L, 432, true, "bla", 'a' :: 'b' :: Nil)))

  test("NaN") {
    val value = Double.NaN

    val serialized = write(value)
    val deserialized = read[Double](serialized)

    assert(java.lang.Double.isNaN(deserialized))
  }

  test("scientific") {
    val value = -1.750470182E9
    val test = value.toString
    val serialized = Seq("-1.750470182E+9", "-1.750470182E9", test)
    val deserialized = serialized.map(read[Double])

    deserialized should contain only value
  }

  test("serialize and deserialize nested case classes") {
    val test: TestCC = TestCC(5, 123L, 432, true, "bla", 'a' :: 'b' :: Nil)
    val test2: TestCC = TestCC(-35, 1L, 432, true, "blsddf sdg  \"{,}[,]\"a", 'a' :: 'b' :: Nil)
    val nested: NestedTestCC = NestedTestCC(-123, test, test2)
    val serialized = write(nested)
    val deserialized = read[NestedTestCC](serialized)

    deserialized should be(nested)
  }

  test("serialize all types") {
    val item = completeItem()
    val serialized = write(item)
    val deserialized = read[CompleteItem](serialized)

    deserialized.unit should be(item.unit)
    deserialized.string should be(item.string)
    deserialized.char should be(item.char)
    deserialized.boolean should be(item.boolean)
    deserialized.byte should be(item.byte)
    deserialized.short should be(item.short)
    deserialized.int should be(item.int)
    deserialized.long should be(item.long)
    deserialized.float should be(item.float)
    deserialized.double should be(item.double)
    deserialized.binary should be(item.binary)
    deserialized.list should be(item.list)
    deserialized.set should be(item.set)
    deserialized.obj should be(item.obj)
    deserialized.map should be(item.map)
  }

  test("handle plain numbers in JSON as Int, Long and Double") {
    val json = "123"
    read[Int](json) should be(123)
    read[Long](json) should be(123)
    read[Double](json) should be(123)

    val maxIntPlusOne: Long = Int.MaxValue.toLong + 1
    val jsonLong = maxIntPlusOne.toString
    intercept[ReadFailure](read[Int](jsonLong))
    read[Long](jsonLong) should be(maxIntPlusOne)
    read[Double](jsonLong) should be(maxIntPlusOne)

    val jsonLongMax = Long.MaxValue.toString
    intercept[ReadFailure](read[Int](jsonLong))
    read[Long](jsonLongMax) should be(Long.MaxValue)
    read[Double](jsonLongMax) should be(Long.MaxValue)

    val jsonDouble = Double.MaxValue.toString
    intercept[ReadFailure](read[Int](jsonDouble))
    intercept[ReadFailure](read[Long](jsonDouble))
    read[Double](jsonDouble) should be(Double.MaxValue)
  }

  test("work with skipping") {
    case class TwoItems(i1: CompleteItem, i2: CompleteItem)
    implicit val skippingCodec = new GenCodec[TwoItems] {
      override def read(input: Input): TwoItems = {
        val obj = input.readObject()
        obj.nextField().skip()
        val i2 = GenCodec.read[CompleteItem](obj.nextField())
        TwoItems(null, i2)
      }


      override def write(output: Output, value: TwoItems): Unit = {
        val obj = output.writeObject()
        GenCodec.write[CompleteItem](obj.writeField("i1"), value.i1)
        GenCodec.write[CompleteItem](obj.writeField("i2"), value.i2)
        obj.finish()
      }
    }

    val item = TwoItems(completeItem(), completeItem())
    val serialized = write(item)
    val deserialized = read[TwoItems](serialized)

    deserialized.i1 should be(null)
    deserialized.i2.unit should be(item.i2.unit)
    deserialized.i2.string should be(item.i2.string)
    deserialized.i2.char should be(item.i2.char)
    deserialized.i2.boolean should be(item.i2.boolean)
    deserialized.i2.byte should be(item.i2.byte)
    deserialized.i2.short should be(item.i2.short)
    deserialized.i2.int should be(item.i2.int)
    deserialized.i2.long should be(item.i2.long)
    deserialized.i2.float should be(item.i2.float)
    deserialized.i2.double should be(item.i2.double)
    deserialized.i2.binary should be(item.i2.binary)
    deserialized.i2.list should be(item.i2.list)
    deserialized.i2.set should be(item.i2.set)
    deserialized.i2.obj should be(item.i2.obj)
    deserialized.i2.map should be(item.i2.map)
  }


  test("serialize and deserialize huge case classes") {
    def cc() = TestCC(Random.nextInt(), Random.nextLong(), Random.nextInt(), Random.nextBoolean(), Random.nextString(Random.nextInt(300)), List.fill(Random.nextInt(300))('a'))
    def ncc() = NestedTestCC(Random.nextInt(), cc(), cc())
    def dncc(counter: Int = 0): DeepNestedTestCC =
      if (counter < 500) DeepNestedTestCC(ncc(), dncc(counter + 1))
      else DeepNestedTestCC(ncc(), null)

    val test: DeepNestedTestCC = dncc()
    val serialized = write(test)
    val deserialized = read[DeepNestedTestCC](serialized)

    deserialized should be(test)
  }
}
