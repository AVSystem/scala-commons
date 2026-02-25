package com.avsystem.commons
package mongo

import com.avsystem.commons.ser.{Nested, Toplevel}

import java.nio.ByteBuffer
import org.bson.codecs.{BsonDocumentCodec, DecoderContext, EncoderContext}
import org.bson.io.BasicOutputBuffer
import org.bson.{BsonArray, BsonBinaryReader, BsonBinaryWriter, BsonDocument, BsonInt32, BsonString}
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, Scope, State, Warmup}

@Warmup(iterations = 10)
@Measurement(iterations = 20)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Thread)
class BsonCodecBenchmark {

  import BsonCodecBenchmark._

  private val something = Toplevel(42, Nested(List(4, 8, 15, 16, 23, 42, 0), 131), "lol")
  private val doc = somethingCodec.toDocument(something)
  private val bytes = binaryEncode(something)

  def binaryEncode(something: Toplevel): Array[Byte] = {
    val output = new BasicOutputBuffer()
    val writer = new BsonBinaryWriter(output)
    val doc = somethingCodec.toDocument(something)
    bsonDocumentCodec.encode(writer, doc.toBson, EncoderContext.builder().build())
    output.toByteArray
  }

  @Benchmark
  def binaryEncoding(): Array[Byte] =
    binaryEncode(something)

  @Benchmark
  def binaryDecoding(): Toplevel = {
    val reader = new BsonBinaryReader(ByteBuffer.wrap(bytes))
    val doc = bsonDocumentCodec.decode(reader, DecoderContext.builder().build())
    somethingCodec.fromDocument(new Doc(doc))
  }

  @Benchmark
  def encoding(): Doc =
    somethingCodec.toDocument(something)

  @Benchmark
  def decoding(): Toplevel =
    somethingCodec.fromDocument(doc)
}

object BsonCodecBenchmark {

  import BsonCodec._

  val bsonDocumentCodec = new BsonDocumentCodec()

  val intKey: DocKey[Int, BsonInt32] = int32.key("int")
  val strKey: DocKey[String, BsonString] = string.key("str")
  val listKey: DocKey[List[Int], BsonArray] = int32.collection[List].key("list")

  val nestedCodec = new DocumentCodec[Nested] {
    override def toDocument(t: Nested): Doc = Doc().put(listKey, t.list).put(intKey, t.int)

    override def fromDocument(doc: Doc) = Nested(
      list = doc.require(listKey),
      int = doc.require(intKey),
    )
  }

  val nestedKey: DocKey[Nested, BsonDocument] = nestedCodec.bsonCodec.key("nested")

  val somethingCodec = new DocumentCodec[Toplevel] {
    override def toDocument(t: Toplevel): Doc = Doc().put(intKey, t.int).put(nestedKey, t.nested).put(strKey, t.str)

    override def fromDocument(doc: Doc): Toplevel = Toplevel(
      int = doc.require(intKey),
      nested = doc.require(nestedKey),
      str = doc.require(strKey),
    )
  }
}
