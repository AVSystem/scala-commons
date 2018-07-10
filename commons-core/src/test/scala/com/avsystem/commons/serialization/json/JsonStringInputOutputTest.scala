package com.avsystem.commons
package serialization.json

import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization.{GenCodec, Input, Output}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._
import org.scalactic.source.Position
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}

import scala.collection.mutable.ListBuffer

class JsonStringInputOutputTest extends FunSuite with SerializationTestUtils with Matchers with PropertyChecks {

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

  def roundtrip[T: GenCodec](name: String)(values: T*)(implicit pos: Position): Unit = {
    test(name) {
      val serialized = values.map(write[T](_))
      val deserialized = serialized.map(read[T](_))
      deserialized shouldEqual values
    }
  }

  roundtrip("integers")(Int.MinValue, -42, 0, 42, Int.MaxValue)

  roundtrip("longs")(Int.MaxValue.toLong + 1, Long.MinValue, -783868188, 0, 783868188, Long.MaxValue)

  roundtrip("doubles")(
    Double.MinPositiveValue, Double.MinValue, -1.750470182E9, Double.MaxValue, Double.PositiveInfinity, Double.NegativeInfinity
  )
  roundtrip("booleans")(false, true)

  roundtrip("strings")("", "a።bc\u0676ąቢść➔Ĳ")

  roundtrip("simple case classes")(TestCC(5, 123L, 432, true, "bla", 'a' :: 'b' :: Nil))

  roundtrip("null")(null)

  roundtrip("dates")(new JDate(0), new JDate(2452323423L))

  test("byte array binary format") {
    val options = JsonOptions(binaryFormat = JsonBinaryFormat.ByteArray)
    assert(write[Array[Byte]](Array(-1, 0, 1), options) == "[-1,0,1]")
    assert(read[Array[Byte]]("[-1,0,1]", options).toSeq == Seq[Byte](-1, 0, 1))
  }

  test("hex string binary format") {
    val options = JsonOptions(binaryFormat = JsonBinaryFormat.HexString)
    assert(write[Array[Byte]](Array(-1, 0, 1), options) == "\"ff0001\"")
    assert(read[Array[Byte]]("\"ff0001\"", options).toSeq == Seq[Byte](-1, 0, 1))
  }

  test("ISO instant date format") {
    val options = JsonOptions(dateFormat = JsonDateFormat.IsoInstant)
    assert(write[JDate](new JDate(0), options) == "\"1970-01-01T00:00:00.000Z\"")
    assert(read[JDate]("\"1970-01-01T00:00:00.000Z\"", options) == new JDate(0))
  }

  test("epoch millis date format") {
    val options = JsonOptions(dateFormat = JsonDateFormat.EpochMillis)
    assert(write[JDate](new JDate(0), options) == "0")
    assert(read[JDate]("0", options) == new JDate(0))
  }

  test("ascii output") {
    val options = JsonOptions(asciiOutput = true)
    assert(write[String]("ąę", options) == "\"\\u0105\\u0119\"")
    assert(read[String]("\"\\u0105\\u0119\"", options) == "ąę")
  }

  test("no big numbers") {
    val options = JsonOptions(bigNumbers = false)
    assert(write[Long](Long.MaxValue, options) == "\"" + Long.MaxValue.toString + "\"")
    assert(read[Long]("\"" + Long.MaxValue.toString + "\"", options) == Long.MaxValue)
  }

  test("indentation") {
    val options = JsonOptions(indentSize = 2)
    val map = Map("a" -> List(1, 2), "b" -> List(3, 4, 5))
    val prettyJson = write[Map[String, List[Int]]](map, options)
    assert(prettyJson ==
      """{
        |  "a":[
        |    1,
        |    2
        |  ],
        |  "b":[
        |    3,
        |    4,
        |    5
        |  ]
        |}""".stripMargin)
    assert(read[Map[String, List[Int]]](prettyJson, options) == map)
  }

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
    val deserialized = serialized.map(read[Double](_))

    deserialized should contain only value
  }

  test("serialize and deserialize nested case classes") {
    val test: TestCC = TestCC(5, 123L, 432, true, "bla", 'a' :: 'b' :: Nil)
    val test2: TestCC = TestCC(-35, 1L, 432, true, "blsddf sdg  \"{,}[,]\"a", 'a' :: 'b' :: Nil)
    val nested: NestedTestCC = NestedTestCC(-123, test, test2)
    val serialized = write(nested)
    val deserialized = read[NestedTestCC](serialized)

    deserialized shouldBe nested
  }

  test("numerical strings") {
    read[String]("\"42\"") shouldBe "42"
    a[ReadFailure] shouldBe thrownBy(read[String]("42"))
    read[Int](json = "\"42\"") shouldBe 42
  }

  test("serialize all types") {
    forAll { item: CompleteItem =>
      val serialized = write(item)
      val deserialized = read[CompleteItem](serialized)

      deserialized.unit shouldBe item.unit
      deserialized.string shouldBe item.string
      deserialized.char shouldBe item.char
      deserialized.boolean shouldBe item.boolean
      deserialized.byte shouldBe item.byte
      deserialized.short shouldBe item.short
      deserialized.int shouldBe item.int
      deserialized.long shouldBe item.long
      deserialized.float shouldBe item.float
      deserialized.double shouldBe item.double
      deserialized.binary shouldBe item.binary
      deserialized.list shouldBe item.list
      deserialized.set shouldBe item.set
      deserialized.obj shouldBe item.obj
      deserialized.map shouldBe item.map
    }
  }

  test("handle plain numbers in JSON as Int, Long and Double") {
    val json = "123"
    read[Int](json) shouldBe 123
    read[Long](json) shouldBe 123
    read[Double](json) shouldBe 123

    val maxIntPlusOne: Long = Int.MaxValue.toLong + 1
    val jsonLong = maxIntPlusOne.toString
    intercept[ReadFailure](read[Int](jsonLong))
    read[Long](jsonLong) shouldBe maxIntPlusOne
    read[Double](jsonLong) shouldBe maxIntPlusOne

    val jsonLongMax = Long.MaxValue.toString
    intercept[ReadFailure](read[Int](jsonLong))
    read[Long](jsonLongMax) shouldBe Long.MaxValue
    read[Double](jsonLongMax) shouldBe Long.MaxValue

    val jsonDouble = Double.MaxValue.toString
    intercept[ReadFailure](read[Int](jsonDouble))
    intercept[ReadFailure](read[Long](jsonDouble))
    read[Double](jsonDouble) shouldBe Double.MaxValue
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

    forAll { (i1: CompleteItem, i2: CompleteItem) =>
      val item = TwoItems(i1, i2)
      val serialized = write(item)
      val deserialized = read[TwoItems](serialized)

      deserialized.i1 shouldBe null
      deserialized.i2.unit shouldBe item.i2.unit
      deserialized.i2.string shouldBe item.i2.string
      deserialized.i2.char shouldBe item.i2.char
      deserialized.i2.boolean shouldBe item.i2.boolean
      deserialized.i2.byte shouldBe item.i2.byte
      deserialized.i2.short shouldBe item.i2.short
      deserialized.i2.int shouldBe item.i2.int
      deserialized.i2.long shouldBe item.i2.long
      deserialized.i2.float shouldBe item.i2.float
      deserialized.i2.double shouldBe item.i2.double
      deserialized.i2.bigInt shouldBe item.i2.bigInt
      deserialized.i2.bigDecimal shouldBe item.i2.bigDecimal
      deserialized.i2.binary shouldBe item.i2.binary
      deserialized.i2.list shouldBe item.i2.list
      deserialized.i2.set shouldBe item.i2.set
      deserialized.i2.obj shouldBe item.i2.obj
      deserialized.i2.map shouldBe item.i2.map
    }
  }

  test("serialize and deserialize huge case classes") {
    implicit val arbTree: Arbitrary[DeepNestedTestCC] =
      Arbitrary {
        def sized(sz: Int): Gen[DeepNestedTestCC] =
          if (sz == 0) for (t <- arbitrary[TestCC]) yield DeepNestedTestCC(t, null)
          else for {
            t <- arbitrary[TestCC]
            n <- sized(sz - 1)
          } yield DeepNestedTestCC(t, n)

        Gen.sized(sz => sized(math.min(sz, 1)))
      }

    forAll { dncc: DeepNestedTestCC =>
      val serialized = write(dncc)
      val deserialized = read[DeepNestedTestCC](serialized)

      deserialized shouldBe dncc
    }
  }
}
