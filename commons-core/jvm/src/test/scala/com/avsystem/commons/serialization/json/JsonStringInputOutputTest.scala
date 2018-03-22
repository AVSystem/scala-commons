package com.avsystem.commons
package serialization.json

import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization.{GenCodec, Output}
import io.circe.testing.ArbitraryInstances
import io.circe.{Json, JsonNumber}
import org.scalatest.concurrent.Eventually
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}

import scala.collection.mutable.ListBuffer

class JsonStringInputOutputTest extends FunSuite with SerializationTestUtils
  with PropertyChecks with ArbitraryInstances with Eventually with Matchers {

  // limit JsonNumbers to Int values
  override def transformJsonNumber(n: JsonNumber): JsonNumber =
    Json.fromInt(n.toBigDecimal.map(_.intValue).getOrElse(0)).asNumber.get

  private def writeJson(json: Json): String = {
    def writeIn(json: Json, output: Output): Unit = json.fold(
      output.writeNull(),
      output.writeBoolean,
      num => num.toInt.fold(output.writeDouble(num.toDouble))(output.writeInt),
      output.writeString,
      arr => {
        val lo = output.writeList()
        arr.foreach(writeIn(_, lo.writeElement()))
        lo.finish()
      },
      obj => {
        val oo = output.writeObject()
        obj.toList.foreach {
          case (key, j) => writeIn(j, oo.writeField(key))
        }
        oo.finish()
      }
    )

    val builder = new JStringBuilder
    val output = new JsonStringOutput(builder)
    writeIn(json, output)
    builder.toString
  }

  private def readJson(json: String): Json = {
    def readIn(input: JsonStringInput): Json = input.jsonType match {
      case "null" => input.readNull(); Json.Null
      case "boolean" => Json.fromBoolean(input.readBoolean())
      case "string" => Json.fromString(input.readString())
      case "integer number" => Json.fromInt(input.readInt())
      case "decimal number" => Json.fromDouble(input.readDouble()).get
      case "list" =>
        val li = input.readList()
        val lb = new ListBuffer[Json]
        while (li.hasNext) {
          lb += readIn(li.nextElement())
        }
        Json.fromValues(lb)
      case "object" =>
        val oi = input.readObject()
        val lb = new ListBuffer[(String, Json)]
        while (oi.hasNext) {
          val fi = oi.nextField()
          lb += ((fi.fieldName, readIn(fi)))
        }
        Json.fromFields(lb)
    }

    readIn(new JsonStringInput(new JsonReader(json)))
  }

  // These tests like to hang up for reasons I have too little patience to investigate
  ignore("write consistency with circe") {
    eventually {
      forAll { json: Json =>
        assert(writeJson(json) == json.noSpaces)
      }
    }
  }

  ignore("read consistency with circe - compact") {
    eventually {
      forAll { json: Json =>
        assert(readJson(json.noSpaces) == json)
      }
    }
  }

  ignore("read consistency with circe - spaced") {
    eventually {
      forAll { json: Json =>
        assert(readJson(json.spaces2) == json)
      }
    }
  }

  ignore("read write round trip") {
    eventually {
      forAll { json: Json =>
        assert(readJson(writeJson(json)) == json)
      }
    }
  }

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
    Double.MinValue, -1.750470182E9, Double.MaxValue, Double.MinPositiveValue, Double.PositiveInfinity, Double.NegativeInfinity)
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
    assume(test == "-1.750470182E9")
    val serialized = Seq("-1.750470182E+9", test)
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


    val brokenDouble = "312,321"
    val i = read[Int](brokenDouble)
    intercept[ReadFailure](i)
    intercept[ReadFailure](read[Long](brokenDouble))
    intercept[ReadFailure](read[Double](brokenDouble))
  }
}
