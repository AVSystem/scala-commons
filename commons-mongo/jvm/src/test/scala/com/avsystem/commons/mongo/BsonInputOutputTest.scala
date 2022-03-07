package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization._
import org.bson._
import org.bson.io.BasicOutputBuffer
import org.bson.json.{JsonMode, JsonWriterSettings}
import org.bson.types.Decimal128
import org.scalactic.source.Position
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.nio.ByteBuffer

class BinaryBsonGenCodecRoundtripTest extends GenCodecRoundtripTest {
  type Raw = Array[Byte]

  def legacyOptionEncoding: Boolean = false

  def writeToOutput(write: Output => Unit): Array[Byte] = {
    val bsonOutput = new BasicOutputBuffer()
    val writer = new BsonBinaryWriter(bsonOutput)
    val output = new BsonWriterOutput(writer, legacyOptionEncoding)
    val objOutput = output.writeObject()
    write(objOutput.writeField("fakeField")) // BSON must be an object
    objOutput.finish()
    bsonOutput.toByteArray
  }

  def createInput(raw: Array[Byte]): Input = {
    val reader = new BsonBinaryReader(ByteBuffer.wrap(raw))
    val input = new BsonReaderInput(reader, legacyOptionEncoding)
    input.readObject().nextField()
  }
}

class DocumentBsonGenCodecRoundtripTest extends GenCodecRoundtripTest {
  type Raw = BsonDocument

  def legacyOptionEncoding: Boolean = false

  def writeToOutput(write: Output => Unit): BsonDocument = {
    val doc = new BsonDocument()
    val writer = new BsonDocumentWriter(doc)
    val output = new BsonWriterOutput(writer, legacyOptionEncoding)
    val objOutput = output.writeObject()
    write(objOutput.writeField("fakeField")) // BSON must be an object
    objOutput.finish()
    doc
  }

  def createInput(raw: BsonDocument): Input = {
    val reader = new BsonDocumentReader(raw)
    val input = new BsonReaderInput(reader, legacyOptionEncoding)
    input.readObject().nextField()
  }
}

class BsonValueGenCodecRoundtripTest extends GenCodecRoundtripTest {
  type Raw = BsonValue

  def legacyOptionEncoding: Boolean = false

  def writeToOutput(write: Output => Unit): BsonValue = {
    var bsonValue: BsonValue = null
    write(new BsonValueOutput(bsonValue = _, legacyOptionEncoding))
    bsonValue
  }

  def createInput(raw: BsonValue): Input =
    new BsonValueInput(raw, legacyOptionEncoding)

  test("Int32 to Long decoding") {
    val input = createInput(new BsonInt32(42))
    val result = input.readSimple().readLong()
    assert(result == 42L)
  }
}

class BsonInputOutputTest extends AnyFunSuite with ScalaCheckPropertyChecks {
  def binaryRoundtrip(sthBefore: SomethingComplex, legacyOptionEncoding: Boolean = false): Unit = {
    val bsonOutput = new BasicOutputBuffer()
    val writer = new BsonBinaryWriter(bsonOutput)
    val output = new BsonWriterOutput(writer, legacyOptionEncoding)

    SomethingComplex.codec.write(output, sthBefore)
    val bytes = bsonOutput.toByteArray

    val reader = new BsonBinaryReader(ByteBuffer.wrap(bytes))
    val input = new BsonReaderInput(reader, legacyOptionEncoding)

    val sthAfter = SomethingComplex.codec.read(input)
    assert(sthAfter == sthBefore)
  }

  def documentRoundtrip(sthBefore: SomethingComplex, legacyOptionEncoding: Boolean = false): Unit = {
    val doc = new BsonDocument()
    val writer = new BsonDocumentWriter(doc)
    val output = new BsonWriterOutput(writer, legacyOptionEncoding)
    SomethingComplex.codec.write(output, sthBefore)

    val expectedDoc = somethingComplexToBson(sthBefore, legacyOptionEncoding)
    assert(doc == expectedDoc)

    val reader = new BsonDocumentReader(doc)
    val input = new BsonReaderInput(reader, legacyOptionEncoding)
    val sthAfter = SomethingComplex.codec.read(input)
    assert(sthAfter == sthBefore)
  }

  def valueEncoding(sthBefore: SomethingComplex, legacyOptionEncoding: Boolean = false): Unit = {
    val doc = BsonValueOutput.write(sthBefore, legacyOptionEncoding).asDocument()
    val expectedDoc = somethingComplexToBson(sthBefore, legacyOptionEncoding)
    assert(doc == expectedDoc)
  }

  test("Roundtrip binary encoding of random objects") {
    forAll(SomethingComplex.gen)(binaryRoundtrip(_))
  }

  test("Roundtrip document encoding of random objects") {
    forAll(SomethingComplex.gen)(documentRoundtrip(_))
  }

  test("Value encoding of random objects") {
    forAll(SomethingComplex.gen)(valueEncoding(_))
  }

  test("Roundtrip binary encoding of random objects + legacy Option encoding") {
    forAll(SomethingComplex.gen)(binaryRoundtrip(_, legacyOptionEncoding = true))
  }

  test("Roundtrip document encoding of random objects + legacy Option encoding") {
    forAll(SomethingComplex.gen)(documentRoundtrip(_, legacyOptionEncoding = true))
  }

  test("Value encoding of random objects + legacy Option encoding") {
    forAll(SomethingComplex.gen)(valueEncoding(_, legacyOptionEncoding = true))
  }

  private case class Wrap[+T](v: T)
  private object Wrap extends HasPolyGenCodec[Wrap]

  def testRoundtripAndRepr[T: GenCodec](value: T, expectedRepr: BsonValue)(implicit pos: Position): Unit = {
    val repr = BsonValueOutput.write(value)
    assert(repr == expectedRepr)
    assert(BsonValueInput.read[T](repr) == value)

    val output = new BasicOutputBuffer
    val writer = new BsonBinaryWriter(output)
    GenCodec.write(new BsonWriterOutput(writer), Wrap(value))
    writer.flush()
    writer.close()

    val bytes = output.toByteArray
    val reprFromBinary = BsonValueUtils.decode(new BsonBinaryReader(ByteBuffer.wrap(bytes))).asDocument().get("v")
    assert(reprFromBinary == expectedRepr)
    assert(GenCodec.read[Wrap[T]](new BsonReaderInput(new BsonBinaryReader(ByteBuffer.wrap(bytes)))).v == value)
  }

  test("Long encoding") {
    testRoundtripAndRepr[Long](1, new BsonInt32(1))
    testRoundtripAndRepr[Long](Int.MaxValue + 1L, new BsonInt64(Int.MaxValue + 1L))
  }

  test("BigInt encoding") {
    testRoundtripAndRepr[BigInt](1, new BsonInt32(1))
    testRoundtripAndRepr[BigInt](BigInt("1"), new BsonInt32(1))
    testRoundtripAndRepr[BigInt](Int.MaxValue + 1L, new BsonInt64(Int.MaxValue + 1L))
    testRoundtripAndRepr[BigInt](BigInt("123123123123123123123"), new BsonDecimal128(Decimal128.parse("123123123123123123123")))
    testRoundtripAndRepr[BigInt](BigInt("123123123123123123123123123123123123123123"), new BsonBinary(Base64.decode("AWnTiveIuE7XC8Q4zzH4T/Oz")))
  }

  test("BigDecimal encoding") {
    testRoundtripAndRepr[BigDecimal](1, new BsonDecimal128(new Decimal128(1)))
    testRoundtripAndRepr[BigDecimal](BigDecimal("0.00001"), new BsonDecimal128(Decimal128.parse("0.00001")))
    testRoundtripAndRepr[BigDecimal](BigDecimal("123123123123.123123123"), new BsonDecimal128(Decimal128.parse("123123123123.123123123")))
    testRoundtripAndRepr[BigDecimal](BigDecimal("123123123123123123.123123123123123123123123"), new BsonBinary(Base64.decode("AWnTiveIuE7XC8Q4zzH4T/OzAAAAGA==")))
  }

  test("All encoding schemes with problematic map keys") {
    forAll(SomethingComplex.gen) { sth =>
      val sthBefore = sth.copy(
        embeddedObject = sth.embeddedObject.copy(
          map = sth.embeddedObject.map + ("$problematic.key" -> "value")
        )
      )

      binaryRoundtrip(sthBefore)
      documentRoundtrip(sthBefore)
      valueEncoding(sthBefore)
    }
  }


  def testMetadata(input: BsonInput)(implicit position: Position): Unit = {
    def testFieldType(input: ObjectInput)(tpe: BsonType): Unit = {
      val fieldInput = input.nextField()
      assert(fieldInput.readMetadata(BsonTypeMetadata).contains(tpe))
      fieldInput.skip()
    }

    assert(input.readMetadata(BsonTypeMetadata).contains(BsonType.DOCUMENT))

    val objectInput = input.readObject()
    assert(input.readMetadata(BsonTypeMetadata).contains(BsonType.DOCUMENT))

    //  string: String,
    //  boolean: Boolean,
    //  int: Int,
    //  long: Long,
    //  timestamp: JDate,
    //  double: Double,
    //  binary: BytesWrapper,
    //  list: List[String],
    //  map: Map[String, String]
    testFieldType(objectInput)(BsonType.STRING)
    testFieldType(objectInput)(BsonType.BOOLEAN)
    testFieldType(objectInput)(BsonType.INT32)
    testFieldType(objectInput)(BsonType.INT64)
    testFieldType(objectInput)(BsonType.DATE_TIME)
    testFieldType(objectInput)(BsonType.DOUBLE)
    testFieldType(objectInput)(BsonType.BINARY)
    testFieldType(objectInput)(BsonType.ARRAY)
    testFieldType(objectInput)(BsonType.DOCUMENT)
  }

  test("BsonDocumentReader type metadata") {
    forAll(SomethingPlain.gen) { sth =>
      val document = somethingToBson(sth)
      val input = new BsonReaderInput(new BsonDocumentReader(document))

      testMetadata(input)
    }
  }

  test("BsonBinaryReader type metadata") {
    forAll(SomethingPlain.gen) { sth =>
      val document = somethingToBson(sth)
      val rawJson = document.toJson(JsonWriterSettings.builder.outputMode(JsonMode.EXTENDED).build)
      val input = new BsonReaderInput(new BsonBinaryReader(RawBsonDocument.parse(rawJson).getByteBuffer.asNIO()))

      testMetadata(input)
    }
  }

  test("BsonBinaryReader peekField") {
    val doc = new BsonDocument()
      .append("str", new BsonString("str"))
      .append("int", new BsonInt32(32))
      .append("boo", new BsonBoolean(false))
      .append("arr", new BsonArray(JList(new BsonInt32(42), new BsonString("foo"))))
      .append("dbl", new BsonDouble(3.14))

    val bof = new BasicOutputBuffer
    val bw = new BsonBinaryWriter(bof)
    BsonValueUtils.encode(bw, doc)
    bw.flush()
    bw.close()

    val br = new BsonBinaryReader(ByteBuffer.wrap(bof.toByteArray))
    val input = new BsonReaderInput(br).readObject()

    locally {
      val pf = input.peekField("str")
      assert(pf.exists(fi => fi.fieldName == "str" && fi.readBsonValue() == new BsonString("str")))
    }
    locally {
      val fi = input.nextField()
      assert(fi.fieldName == "str")
      fi.skip()
    }
    locally {
      val pf = input.peekField("boo")
      assert(pf.exists(fi => fi.fieldName == "boo" && fi.readBsonValue() == new BsonBoolean(false)))
    }
    locally {
      assert(input.peekField("not").isEmpty)
    }
    locally {
      val pf = input.peekField("dbl")
      assert(pf.exists(fi => fi.fieldName == "dbl" && fi.readBsonValue() == new BsonDouble(3.14)))
    }
    locally {
      val fi = input.nextField()
      assert(fi.fieldName == "int")
      fi.skip()
    }
    locally {
      val pf = input.peekField("str")
      assert(pf.exists(fi => fi.fieldName == "str" && fi.readBsonValue() == new BsonString("str")))
    }
  }

  def listToBson[T](list: List[T])(converter: T => BsonValue) = new BsonArray(list.map(converter).asJava)

  def mapToBson[T](map: Map[String, T])(valueConverter: T => BsonValue): BsonDocument = {
    val doc = new BsonDocument()
    for ((key, value) <- map) {
      doc.put(KeyEscaper.escape(key), valueConverter(value))
    }
    doc
  }

  def somethingToBson(s: SomethingPlain): BsonDocument = {
    new BsonDocument()
      .append("string", new BsonString(s.string))
      .append("boolean", new BsonBoolean(s.boolean))
      .append("int", new BsonInt32(s.int))
      .append("long", new BsonInt64(s.long))
      .append("timestamp", new BsonDateTime(s.timestamp.getTime))
      .append("double", new BsonDouble(s.double))
      .append("binary", new BsonBinary(s.binary.bytes))
      .append("list", listToBson(s.list)(new BsonString(_)))
      .append("map", mapToBson(s.map)(new BsonString(_)))
  }

  def somethingComplexToBson(sc: SomethingComplex, legacyOptionEncoding: Boolean = false): BsonDocument = {
    new BsonDocument()
      .append("embeddedObject", somethingToBson(sc.embeddedObject))
      .append("complexList", new BsonArray(sc.complexList.map(somethingToBson).asJava))
      .append("nestedList", listToBson(sc.nestedList)(listToBson(_)(new BsonString(_))))
      .append("nestedComplexList", listToBson(sc.nestedComplexList)(listToBson(_)(somethingToBson)))
      .append("option",
        if (legacyOptionEncoding) listToBson(sc.option.toList)(new BsonInt32(_))
        else sc.option.fold[BsonValue](new BsonNull)(new BsonInt32(_))
      )
  }
}
