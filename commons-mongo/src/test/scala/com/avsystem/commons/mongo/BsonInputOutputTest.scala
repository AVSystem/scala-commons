package com.avsystem.commons
package mongo

import java.nio.ByteBuffer

import com.avsystem.commons.serialization.{GenCodecRoundtripTest, Input, ObjectInput, Output}
import org.bson._
import org.bson.io.BasicOutputBuffer
import org.scalactic.source.Position
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

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
}

class BsonInputOutputTest extends FunSuite with PropertyChecks {
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

    assert(input.readMetadata(BsonTypeMetadata).isEmpty)

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
      val input = new BsonReaderInput(new BsonBinaryReader(RawBsonDocument.parse(document.toJson).getByteBuffer.asNIO()))

      testMetadata(input)
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
