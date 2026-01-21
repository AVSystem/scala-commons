package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization.json.{JsonStringInput, JsonStringOutput}
import com.avsystem.commons.serialization.{GenCodec, HasGenCodecWithDeps}
import org.bson._
import org.bson.json.JsonReader
import org.bson.types.{Decimal128, ObjectId}
import org.scalactic.source.Position
import org.scalatest.funsuite.AnyFunSuite

case class AllTypesInABag(
  array: BsonArray,
  binary: BsonBinary,
  boolean: BsonBoolean,
  dateTime: BsonDateTime,
  document: BsonDocument,
  decimal128: BsonDecimal128,
  double: BsonDouble,
  int32: BsonInt32,
  int64: BsonInt64,
  objectId: BsonObjectId,
  string: BsonString,
  value: BsonValue,
)
object AllTypesInABag extends HasGenCodecWithDeps[BsonGenCodecs.type, AllTypesInABag]

class BsonValueCodecsTest extends AnyFunSuite with BsonGenCodecs {
  test("codec roundtrip") {
    val doc = new BsonDocument(
      JList(
        new BsonElement("someInt64", new BsonInt64(64)),
        new BsonElement("someString", new BsonString("some")),
      )
    )

    val bag = AllTypesInABag(
      new BsonArray(JList(BsonBoolean.TRUE, new BsonInt32(131))),
      new BsonBinary(Array.tabulate[Byte](16)(_.toByte)),
      BsonBoolean.FALSE,
      new BsonDateTime(313),
      doc,
      new BsonDecimal128(new Decimal128(1331)),
      new BsonDouble(1.31),
      new BsonInt32(132),
      new BsonInt64(164),
      new BsonObjectId(new ObjectId("12345678901234567890ABCD")),
      new BsonString("sss"),
      doc,
    )

    var value: BsonValue| Null = null
    val output = new BsonValueOutput(value = _)
    GenCodec.write(output, bag)

    assert(value.isInstanceOf[BsonDocument])
    val outDoc = value.asInstanceOf[BsonDocument]
    assert(outDoc.getArray("array") === bag.array)
    assert(outDoc.getBinary("binary") === bag.binary)
    assert(outDoc.getBoolean("boolean") === bag.boolean)
    assert(outDoc.getDateTime("dateTime") === bag.dateTime)
    assert(outDoc.getDocument("document") === bag.document)
    assert(outDoc.getDecimal128("decimal128") === bag.decimal128)
    assert(outDoc.getDouble("double") === bag.double)
    assert(outDoc.getInt32("int32") === bag.int32)
    assert(outDoc.getInt64("int64") === bag.int64)
    assert(outDoc.getObjectId("objectId") === bag.objectId)
    assert(outDoc.getString("string") === bag.string)
    assert(outDoc.get("value") === bag.value)

    val input = new BsonValueInput(value)
    assert(GenCodec.read[AllTypesInABag](input) === bag)
  }

  test("null handling") {
    val reader = new JsonReader("""{"key": null}""")
    val input = new BsonReaderInput(reader)
    val document = GenCodec.read[BsonDocument](input)
    assert(document === new BsonDocument("key", BsonNull.VALUE))

    var decoded: BsonValue| Null = null
    val output = new BsonValueOutput(decoded = _)
    GenCodec.write(output, document)
    assert(decoded === document)
  }

  test("null in doc in array") {
    val reader = new JsonReader("""[{"key": null}]""")
    val input = new BsonReaderInput(reader)
    val array = GenCodec.read[BsonArray](input)
    assert(array === new BsonArray(JList(new BsonDocument("key", BsonNull.VALUE))))
  }

  def testJsonRoundtrip[T: GenCodec](value: T)(implicit pos: Position): Unit = {
    val json = JsonStringOutput.write(value)
    val readValue = JsonStringInput.read[T](json)
    assert(value == readValue)
  }

  test("JSON string roundtrip for BsonValue") {
    testJsonRoundtrip(new BsonBoolean(true))
    testJsonRoundtrip(new BsonInt32(42))
    testJsonRoundtrip(new BsonInt64(42))
    testJsonRoundtrip(new BsonDocument("v", new BsonInt32(42)))
  }
}
