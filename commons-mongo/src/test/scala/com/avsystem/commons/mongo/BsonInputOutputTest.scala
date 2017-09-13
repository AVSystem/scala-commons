package com.avsystem.commons
package mongo

import java.nio.ByteBuffer

import org.bson._
import org.bson.io.BasicOutputBuffer
import org.scalatest.FunSuite

class BsonInputOutputTest extends FunSuite {
  test("Roundtrip binary encoding of random objects") {
    100 times {
      val sthBefore = SomethingComplex.random

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
  }

  test("Roundtrip document encoding of random objects") {
    100 times {
      val sthBefore = SomethingComplex.random
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
  }

  test("Value encoding of random objects") {
    100 times {
      val sthBefore = SomethingComplex.random
      val output = new BsonValueOutput()
      SomethingComplex.codec.write(output, sthBefore)

      val doc = output.value.asDocument()
      val expectedDoc = somethingComplexToBson(sthBefore)
      assert(doc === expectedDoc)
    }
  }

  def listToBson[T](list: List[T])(converter: T => BsonValue) = new BsonArray(list.map(converter).asJava)

  def mapToBson[T](map: Map[String, T])(valueConverter: T => BsonValue): BsonDocument = {
    val doc = new BsonDocument()
    for ((key, value) <- map) {
      doc.put(key, valueConverter(value))
    }
    doc
  }

  def somethingToBson(s: Something): BsonDocument = {
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
