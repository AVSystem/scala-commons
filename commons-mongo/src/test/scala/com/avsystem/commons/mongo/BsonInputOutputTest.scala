package com.avsystem.commons
package mongo

import java.nio.ByteBuffer

import org.bson._
import org.bson.io.BasicOutputBuffer
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

class BsonInputOutputTest extends FunSuite with PropertyChecks {
  def binaryRoundtrip(sthBefore: SomethingComplex): Unit = {
    val bsonOutput = new BasicOutputBuffer()
    val writer = new BsonBinaryWriter(bsonOutput)
    val output = new BsonWriterOutput(writer)

    SomethingComplex.codec.write(output, sthBefore)
    val bytes = bsonOutput.toByteArray

    val reader = new BsonBinaryReader(ByteBuffer.wrap(bytes))
    val input = new BsonReaderInput(reader)

    val sthAfter = SomethingComplex.codec.read(input)
    assert(sthAfter === sthBefore)
  }

  def documentRoundtrip(sthBefore: SomethingComplex): Unit = {
    val doc = new BsonDocument()
    val writer = new BsonDocumentWriter(doc)
    val output = new BsonWriterOutput(writer)
    SomethingComplex.codec.write(output, sthBefore)

    val expectedDoc = somethingComplexToBson(sthBefore)
    assert(doc === expectedDoc)

    val reader = new BsonDocumentReader(doc)
    val input = new BsonReaderInput(reader)
    val sthAfter = SomethingComplex.codec.read(input)
    assert(sthAfter === sthBefore)
  }

  def valueEncoding(sthBefore: SomethingComplex): Unit = {
    val output = new BsonValueOutput()
    SomethingComplex.codec.write(output, sthBefore)

    val doc = output.value.asDocument()
    val expectedDoc = somethingComplexToBson(sthBefore)
    assert(doc === expectedDoc)
  }

  test("Roundtrip binary encoding of random objects") {
    forAll(SomethingComplex.gen)(binaryRoundtrip)
  }

  test("Roundtrip document encoding of random objects") {
    forAll(SomethingComplex.gen)(documentRoundtrip)
  }

  test("Value encoding of random objects") {
    forAll(SomethingComplex.gen)(valueEncoding)
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

  def somethingComplexToBson(sc: SomethingComplex): BsonDocument = {
    new BsonDocument()
      .append("embeddedObject", somethingToBson(sc.embeddedObject))
      .append("complexList", new BsonArray(sc.complexList.map(somethingToBson).asJava))
      .append("nestedList", listToBson(sc.nestedList)(listToBson(_)(new BsonString(_))))
      .append("nestedComplexList", listToBson(sc.nestedComplexList)(listToBson(_)(somethingToBson)))
  }
}
