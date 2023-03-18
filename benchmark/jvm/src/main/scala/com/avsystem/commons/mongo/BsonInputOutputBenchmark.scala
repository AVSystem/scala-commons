package com.avsystem.commons
package mongo

import java.io.StringWriter
import java.nio.ByteBuffer

import com.avsystem.commons.rpc.akka.serialization.{Nested, Something}
import org.bson.io.BasicOutputBuffer
import org.bson.json.{JsonReader, JsonWriter}
import org.bson.{BsonBinaryReader, BsonBinaryWriter, BsonDocument, BsonDocumentReader, BsonDocumentWriter, BsonReader, BsonValue, BsonWriter}
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, Scope, State, Warmup}

@Warmup(iterations = 10)
@Measurement(iterations = 20)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Thread)
class BsonInputOutputBenchmark {
  private val something = Something(42, Nested(List(4, 8, 15, 16, 23, 42, 0), 131), "lol")
  private val bytes = binaryEncode(something)
  private val doc = documentEncode(something)
  private val json = jsonEncode(something)

  def write(something: Something, bsonWriter: BsonWriter): Unit = {
    val output = new BsonWriterOutput(bsonWriter)
    Something.codec.write(output, something)
  }

  def binaryEncode(something: Something): Array[Byte] = {
    val bsonOutput = new BasicOutputBuffer()
    write(something, new BsonBinaryWriter(bsonOutput))
    bsonOutput.toByteArray
  }

  def documentEncode(something: Something): BsonDocument = {
    val doc = new BsonDocument()
    write(something, new BsonDocumentWriter(doc))
    doc
  }

  def jsonEncode(something: Something): String = {
    val stringWriter = new StringWriter()
    write(something, new JsonWriter(stringWriter))
    stringWriter.toString
  }

  @Benchmark
  def binaryEncoding(): Array[Byte] = {
    binaryEncode(something)
  }

  @Benchmark
  def documentEncoding(): BsonDocument = {
    documentEncode(something)
  }

  @Benchmark
  def jsonEncoding(): String = {
    jsonEncode(something)
  }

  @Benchmark
  def valueEncoding(): BsonValue = {
    BsonValueOutput.write(something)
  }

  def read(bsonReader: BsonReader): Something = {
    val input = new BsonReaderInput(bsonReader)
    Something.codec.read(input)
  }

  @Benchmark
  def binaryDecoding(): Something = {
    read(new BsonBinaryReader(ByteBuffer.wrap(bytes)))
  }

  @Benchmark
  def documentDecoding(): Something = {
    read(new BsonDocumentReader(doc))
  }

  @Benchmark
  def jsonDecoding(): Something = {
    read(new JsonReader(json))
  }
}
