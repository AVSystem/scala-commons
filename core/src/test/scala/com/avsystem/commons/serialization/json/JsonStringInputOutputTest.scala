// package com.avsystem.commons
// package serialization.json

// import com.avsystem.commons.serialization.CodecTestData.{CustomizedSeal, FlatSealedBase, OtherCustomCase}
// import com.avsystem.commons.serialization.GenCodec.ReadFailure
// import com.avsystem.commons.serialization.*
// import org.scalacheck.Arbitrary.arbitrary
// import org.scalacheck.*
// import org.scalactic.source.Position
// import org.scalatest.funsuite.AnyFunSuite
// import org.scalatest.matchers.should.Matchers
// import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

// import scala.collection.mutable.ListBuffer

// class JsonStringInputOutputTest
//   extends AnyFunSuite with SerializationTestUtils with Matchers with ScalaCheckPropertyChecks {

//   import JsonStringInput.read
//   import JsonStringOutput.write

//   test("raw json list test") {
//     val jsons = List("123", "null", "\"str\"", "4.5", "[1,2,3]", "{\"a\": 123, \"b\": 3.14}")
//     val sb = new JStringBuilder
//     val output = new JsonStringOutput(sb)
//     val lo = output.writeList()
//     jsons.foreach(json => lo.writeElement().writeRawJson(json))
//     lo.finish()
//     val jsonList = sb.toString

//     assert(jsonList == jsons.mkString("[", ",", "]"))

//     val input = new JsonStringInput(new JsonReader(jsonList))
//     val li = input.readList()
//     val resBuilder = new ListBuffer[String]
//     while li.hasNext do {
//       resBuilder += li.nextElement().readRawJson()
//     }
//     assert(resBuilder.result() == jsons)
//   }

//   test("raw json object test") {
//     val jsons = IListMap(
//       "a" -> "123",
//       "b" -> "null",
//       "c" -> "\"str\"",
//       "d" -> "4.5",
//       "e" -> "[1,2,3]",
//       "f" -> "{\"a\": 123, \"b\": 3.14}",
//     )
//     val sb = new JStringBuilder
//     val output = new JsonStringOutput(sb)
//     val oo = output.writeObject()
//     jsons.foreach { case (fn, fv) => oo.writeField(fn).writeRawJson(fv) }
//     oo.finish()
//     val jsonList = sb.toString

//     assert(jsonList == jsons.map { case (k, v) => s""""$k":$v""" }.mkString("{", ",", "}"))

//     val input = new JsonStringInput(new JsonReader(jsonList))
//     val oi = input.readObject()
//     val resBuilder = IListMap.newBuilder[String, String]
//     while oi.hasNext do {
//       val fi = oi.nextField()
//       resBuilder += ((fi.fieldName, fi.readRawJson()))
//     }
//     assert(resBuilder.result() == jsons)
//   }

//   def roundtrip[T: GenCodec](name: String)(values: T*)(implicit pos: Position): Unit = {
//     test(name) {
//       val serialized = values.map(write[T](_))
//       val deserialized = serialized.map(read[T](_))
//       deserialized shouldEqual values
//     }
//   }

//   roundtrip("integers")(Int.MinValue, -42, 0, 42, Int.MaxValue)

//   roundtrip("longs")(Int.MaxValue.toLong + 1, Long.MinValue, -783868188, 0, 783868188, Long.MaxValue)

//   roundtrip("floats")(
//     Float.MinPositiveValue,
//     Float.MinValue,
//     -1.75047014e9f,
//     Float.MaxValue,
//     Float.PositiveInfinity,
//     Float.NegativeInfinity,
//   )

//   roundtrip("doubles")(
//     Double.MinPositiveValue,
//     Double.MinValue,
//     -1.750470182e9,
//     Double.MaxValue,
//     Double.PositiveInfinity,
//     Double.NegativeInfinity,
//   )

//   roundtrip("booleans")(false, true)

//   roundtrip("strings")("", "a።bc\u0676ąቢść➔Ĳ")

//   roundtrip("simple case classes")(TestCC(5, 123L, 432, b = true, "bla", 'a' :: 'b' :: Nil))

//   roundtrip("null")(null)

//   roundtrip("dates")(new JDate(0), new JDate(2452323423L))

//   test("reading decimal numbers as integers") {
//     assert(read[Byte]("1e2") == 100)
//     assert(read[Short]("3e4") == 30000)
//     assert(read[Int]("3e5") == 300000)
//     assert(read[Long]("3e5") == 300000L)
//     assert(read[BigInt]("3e5") == BigInt(300000))
//   }

//   test("attempting to read number from not a number or string") {
//     intercept[ReadFailure](read[Byte]("true"))
//     intercept[ReadFailure](read[Short]("true"))
//     intercept[ReadFailure](read[Int]("true"))
//     intercept[ReadFailure](read[Long]("true"))
//     intercept[ReadFailure](read[BigInt]("true"))
//     intercept[ReadFailure](read[Float]("true"))
//     intercept[ReadFailure](read[Double]("true"))
//     intercept[ReadFailure](read[BigDecimal]("true"))
//   }

//   test("byte array binary format") {
//     val options = JsonOptions(binaryFormat = JsonBinaryFormat.ByteArray)
//     assert(write[Array[Byte]](Array(-1, 0, 1), options) == "[-1,0,1]")
//     assert(read[Array[Byte]]("[-1,0,1]", options).toSeq == Seq[Byte](-1, 0, 1))
//   }

//   test("hex string binary format") {
//     val options = JsonOptions(binaryFormat = JsonBinaryFormat.HexString)
//     assert(write[Array[Byte]](Array(-1, 0, 1), options) == "\"ff0001\"")
//     assert(read[Array[Byte]]("\"ff0001\"", options).toSeq == Seq[Byte](-1, 0, 1))
//   }

//   test("ISO instant date format") {
//     val options = JsonOptions(dateFormat = JsonDateFormat.IsoInstant)
//     assert(write[JDate](new JDate(1), options) == "\"1970-01-01T00:00:00.001Z\"")
//     assert(read[JDate]("\"1970-01-01T00:00:00.001Z\"", options) == new JDate(1))
//   }

//   test("epoch millis date format") {
//     val options = JsonOptions(dateFormat = JsonDateFormat.EpochMillis)
//     assert(write[JDate](new JDate(0), options) == "0")
//     assert(read[JDate]("0", options) == new JDate(0))
//   }

//   test("ascii output") {
//     val options = JsonOptions(asciiOutput = true)
//     assert(write[String]("ąę", options) == "\"\\u0105\\u0119\"")
//     assert(read[String]("\"\\u0105\\u0119\"", options) == "ąę")
//     intercept[ReadFailure](read[String]("\"\\x0105\""))
//   }

//   test("indentation and spacing") {
//     val options = JsonOptions.Pretty
//     val map = Map("a" -> List(1, 2), "b" -> List(3, 4, 5))
//     val prettyJson = write[Map[String, List[Int]]](map, options)
//     assert(prettyJson == """{
//                            |  "a": [
//                            |    1,
//                            |    2
//                            |  ],
//                            |  "b": [
//                            |    3,
//                            |    4,
//                            |    5
//                            |  ]
//                            |}""".stripMargin)
//     assert(read[Map[String, List[Int]]](prettyJson, options) == map)
//   }

//   test("whitespace skipping") {
//     val json = """ { "a" : [ 1 , 2 ] , "b" : [ ] } """
//     assert(read[Map[String, List[Int]]](json) == Map("a" -> List(1, 2), "b" -> Nil))
//   }

//   test("NaN") {
//     assert(java.lang.Float.isNaN(read[Float](write(Float.NaN))))
//     assert(java.lang.Double.isNaN(read[Double](write(Double.NaN))))
//   }

//   test("scientific") {
//     val value = -1.750470182e9
//     val test = value.toString
//     val serialized = Seq("-1.750470182E+9", "-1.750470182E9", test)
//     val deserialized = serialized.map(read[Double](_))

//     deserialized should contain only value
//   }

//   test("deserialize floats precisely") {
//     val values = Seq("1.199999988079071", "3.4028235677973366E38", "7.006492321624086E-46")
//     assert(values.map(read[Float](_)) == values.map(_.toFloat))
//   }

//   test("serialize floats succinctly") {
//     val values = Seq(1.1999999f, 3.4e38f, 1.4e-45f)
//     assert(values.map(write[Float](_)) == values.map(_.toString.replace('E', 'e')))
//   }

//   test("serialize and deserialize nested case classes") {
//     val test: TestCC = TestCC(5, 123L, 432, b = true, "bla", 'a' :: 'b' :: Nil)
//     val test2: TestCC = TestCC(-35, 1L, 432, b = true, "blsddf sdg  \"{,}[,]\"a", 'a' :: 'b' :: Nil)
//     val nested: NestedTestCC = NestedTestCC(-123, test, test2)
//     val serialized = write(nested)
//     val deserialized = read[NestedTestCC](serialized)

//     deserialized shouldBe nested
//   }

//   test("numerical strings") {
//     read[String]("\"42\"") shouldBe "42"
//     a[ReadFailure] shouldBe thrownBy(read[String]("42"))
//     read[Int](json = "\"42\"") shouldBe 42
//   }

//   test("serialize all types") {
//     forAll { (item: CompleteItem) =>
//       val serialized = write(item)
//       val deserialized = read[CompleteItem](serialized)

//       deserialized.unit shouldBe item.unit
//       deserialized.string shouldBe item.string
//       deserialized.char shouldBe item.char
//       deserialized.boolean shouldBe item.boolean
//       deserialized.byte shouldBe item.byte
//       deserialized.short shouldBe item.short
//       deserialized.int shouldBe item.int
//       deserialized.long shouldBe item.long
//       deserialized.float shouldBe item.float
//       deserialized.double shouldBe item.double
//       deserialized.binary shouldBe item.binary
//       deserialized.list shouldBe item.list
//       deserialized.set shouldBe item.set
//       deserialized.obj shouldBe item.obj
//       deserialized.map shouldBe item.map
//     }
//   }

//   test("handle plain numbers in JSON as Byte, Short, Int, Long, Double, BigInt and BigDecimal") {
//     val json = "123"
//     read[Byte](json) shouldBe 123
//     read[Short](json) shouldBe 123
//     read[Int](json) shouldBe 123
//     read[Long](json) shouldBe 123
//     read[Double](json) shouldBe 123
//     read[BigInt](json) shouldBe BigInt(123)
//     read[BigDecimal](json) shouldBe BigDecimal(123)

//     val maxBytePlusOne: Int = Byte.MaxValue + 1
//     val jsonShort = maxBytePlusOne.toString
//     intercept[ReadFailure](read[Byte](jsonShort))
//     read[Short](jsonShort) shouldBe maxBytePlusOne
//     read[Int](jsonShort) shouldBe maxBytePlusOne
//     read[Long](jsonShort) shouldBe maxBytePlusOne
//     read[Double](jsonShort) shouldBe maxBytePlusOne
//     read[BigInt](jsonShort) shouldBe BigInt(maxBytePlusOne)
//     read[BigDecimal](jsonShort) shouldBe BigDecimal(maxBytePlusOne)

//     val maxShortPlusOne: Int = Short.MaxValue + 1
//     val jsonInt = maxShortPlusOne.toString
//     intercept[ReadFailure](read[Byte](jsonInt))
//     intercept[ReadFailure](read[Short](jsonInt))
//     read[Int](jsonInt) shouldBe maxShortPlusOne
//     read[Long](jsonInt) shouldBe maxShortPlusOne
//     read[Double](jsonInt) shouldBe maxShortPlusOne
//     read[BigInt](jsonInt) shouldBe BigInt(maxShortPlusOne)
//     read[BigDecimal](jsonInt) shouldBe BigDecimal(maxShortPlusOne)

//     val maxIntPlusOne: Long = Int.MaxValue.toLong + 1
//     val jsonLong = maxIntPlusOne.toString
//     intercept[ReadFailure](read[Byte](jsonLong))
//     intercept[ReadFailure](read[Short](jsonLong))
//     intercept[ReadFailure](read[Int](jsonLong))
//     read[Long](jsonLong) shouldBe maxIntPlusOne
//     read[Double](jsonLong) shouldBe maxIntPlusOne
//     read[BigInt](jsonLong) shouldBe BigInt(maxIntPlusOne)
//     read[BigDecimal](jsonLong) shouldBe BigDecimal(maxIntPlusOne)

//     val jsonLongMax = Long.MaxValue.toString
//     intercept[ReadFailure](read[Byte](jsonLongMax))
//     intercept[ReadFailure](read[Short](jsonLongMax))
//     intercept[ReadFailure](read[Int](jsonLongMax))
//     read[Long](jsonLongMax) shouldBe Long.MaxValue
//     read[Double](jsonLongMax) shouldBe Long.MaxValue
//     read[BigInt](jsonLongMax) shouldBe BigInt(Long.MaxValue)
//     read[BigDecimal](jsonLongMax) shouldBe BigDecimal(Long.MaxValue)

//     val overMaxLong = BigInt(Long.MaxValue) + 1
//     val overMaxLongStr = overMaxLong.toString
//     intercept[ReadFailure](read[Byte](overMaxLongStr))
//     intercept[ReadFailure](read[Short](overMaxLongStr))
//     intercept[ReadFailure](read[Int](overMaxLongStr))
//     intercept[ReadFailure](read[Long](overMaxLongStr))
//     read[Double](overMaxLongStr) shouldBe overMaxLong.toDouble
//     read[BigInt](overMaxLongStr) shouldBe overMaxLong
//     read[BigDecimal](overMaxLongStr) shouldBe BigDecimal(overMaxLong)

//     val jsonDouble = Double.MaxValue.toString
//     intercept[ReadFailure](read[Byte](jsonDouble))
//     intercept[ReadFailure](read[Short](jsonDouble))
//     intercept[ReadFailure](read[Int](jsonDouble))
//     intercept[ReadFailure](read[Long](jsonDouble))
//     read[Double](jsonDouble) shouldBe Double.MaxValue
//     read[BigInt](jsonDouble) shouldBe BigDecimal(Double.MaxValue).toBigInt
//     read[BigDecimal](jsonDouble) shouldBe BigDecimal(Double.MaxValue)

//     val jsonSmallDouble = Double.MinPositiveValue.toString
//     intercept[ReadFailure](read[Byte](jsonSmallDouble))
//     intercept[ReadFailure](read[Short](jsonSmallDouble))
//     intercept[ReadFailure](read[Int](jsonSmallDouble))
//     intercept[ReadFailure](read[Long](jsonSmallDouble))
//     read[Double](jsonSmallDouble) shouldBe Double.MinPositiveValue
//     intercept[ReadFailure](read[BigInt](jsonSmallDouble))
//     read[BigDecimal](jsonSmallDouble) shouldBe BigDecimal(Double.MinPositiveValue)
//   }

//   test("work with skipping") {
//     case class TwoItems(i1: CompleteItem, i2: CompleteItem)
//     implicit val skippingCodec: GenCodec[TwoItems] = new GenCodec[TwoItems] {
//       override def read(input: Input): TwoItems = {
//         val obj = input.readObject()
//         obj.nextField().skip()
//         val i2 = GenCodec.read[CompleteItem](obj.nextField())
//         TwoItems(null, i2)
//       }

//       override def write(output: Output, value: TwoItems): Unit = {
//         val obj = output.writeObject()
//         GenCodec.write[CompleteItem](obj.writeField("i1"), value.i1)
//         GenCodec.write[CompleteItem](obj.writeField("i2"), value.i2)
//         obj.finish()
//       }
//     }

//     forAll { (i1: CompleteItem, i2: CompleteItem) =>
//       val item = TwoItems(i1, i2)
//       val serialized = write(item)
//       val deserialized = read[TwoItems](serialized)

//       deserialized.i1 shouldBe null
//       deserialized.i2.unit shouldBe item.i2.unit
//       deserialized.i2.string shouldBe item.i2.string
//       deserialized.i2.char shouldBe item.i2.char
//       deserialized.i2.boolean shouldBe item.i2.boolean
//       deserialized.i2.byte shouldBe item.i2.byte
//       deserialized.i2.short shouldBe item.i2.short
//       deserialized.i2.int shouldBe item.i2.int
//       deserialized.i2.long shouldBe item.i2.long
//       deserialized.i2.float shouldBe item.i2.float
//       deserialized.i2.double shouldBe item.i2.double
//       deserialized.i2.bigInt shouldBe item.i2.bigInt
//       deserialized.i2.bigDecimal shouldBe item.i2.bigDecimal
//       deserialized.i2.binary shouldBe item.i2.binary
//       deserialized.i2.list shouldBe item.i2.list
//       deserialized.i2.set shouldBe item.i2.set
//       deserialized.i2.obj shouldBe item.i2.obj
//       deserialized.i2.map shouldBe item.i2.map
//     }
//   }

//   test("serialize and deserialize huge case classes") {
//     implicit val arbTree: Arbitrary[DeepNestedTestCC] =
//       Arbitrary {
//         def sized(sz: Int): Gen[DeepNestedTestCC] =
//           if sz == 0 then for t <- arbitrary[TestCC] yield DeepNestedTestCC(t, null)
//           else
//             for {
//               t <- arbitrary[TestCC]
//               n <- sized(sz - 1)
//             } yield DeepNestedTestCC(t, n)

//         Gen.sized(sz => sized(math.min(sz, 1)))
//       }

//     forAll { (dncc: DeepNestedTestCC) =>
//       val serialized = write(dncc)
//       val deserialized = read[DeepNestedTestCC](serialized)

//       deserialized shouldBe dncc
//     }
//   }

//   case class NestedOne(@transientDefault n: Option[NestedOne] = None)
//   object NestedOne extends HasGenCodec[NestedOne]

//   case class NestedTwo(@transientDefault @whenAbsent(None) n: Option[NestedTwo])
//   object NestedTwo extends HasGenCodec[NestedTwo]

//   test("serializing recursively defined case classes with optional field") {
//     assert(JsonStringOutput.write(NestedOne()) == "{}")
//     assert(JsonStringOutput.write(NestedTwo(None)) == "{}")
//   }

//   test("read escaped slash") {
//     assert(JsonStringInput.read[String](""""a\/b"""") == "a/b")
//   }

//   test("peeking fields - empty object") {
//     val json = """{}"""
//     val oi = new JsonStringInput(new JsonReader(json)).readObject()
//     assert(oi.peekField("x").isEmpty)
//     assert(!oi.hasNext)
//   }

//   test("peeking fields - non empty object") {
//     val json = """{"a": 25, "b": true, "c": [1,2,3], "d": {}}"""
//     val oi = new JsonStringInput(new JsonReader(json)).readObject()
//     assert(oi.peekField("a").map(GenCodec.read[Int]).contains(25))
//     assert(oi.peekField("b").map(GenCodec.read[Boolean]).contains(true))
//     assert(oi.nextField().fieldName == "a") // peeking should not affect `nextField`
//     assert(oi.peekField("d").map(GenCodec.read[Map[String, String]]).contains(Map.empty))
//     assert(oi.peekField("c").map(GenCodec.read[List[Int]]).contains(List(1, 2, 3)))
//     assert(oi.peekField("x").isEmpty)
//     assert(oi.peekField("a").map(GenCodec.read[Int]).contains(25))
//     assert(oi.nextField().fieldName == "b") // peeking should not affect `nextField`
//   }

//   test("reading flat sealed hierarchy with changed field order") {
//     val json = """{"_id": "foo", "int": 31, "_case": "FirstCase"}"""
//     assert(JsonStringInput.read[FlatSealedBase](json) == FlatSealedBase.FirstCase("foo", 31))
//   }

//   test("reading flat sealed hierarchy with changed field order & custom case field name") {
//     val json = """{"flag": false, "kejs": "OtherCustomCase", "value": 41}"""
//     assert(JsonStringInput.read[CustomizedSeal](json) == OtherCustomCase(41, flag = false))
//   }
// }
