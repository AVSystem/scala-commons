package com.avsystem.commons
package ser

import com.avsystem.commons.serialization.json.{JsonStringInput, JsonStringOutput}
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, Warmup}

@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 2)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
abstract class JsonSerializationBenchmark

class JsonEncodingBenchmark extends JsonSerializationBenchmark {
  @Benchmark
  def encodeCCCirce: Json =
    Something.Example.asJson

  @Benchmark
  def encodeCCGenCodec: Json =
    CirceJsonOutput.write(Something.Example)

  @Benchmark
  def encodeSHCirce: Json =
    SealedStuff.ExampleList.asJson

  @Benchmark
  def encodeSHGenCodec: Json =
    CirceJsonOutput.write(SealedStuff.ExampleList)

  @Benchmark
  def encodeFoosCirce: Json =
    Foo.ExampleMap.asJson

  @Benchmark
  def encodeFoosGenCodec: Json =
    CirceJsonOutput.write(Foo.ExampleMap)
}

class JsonDecodingBenchmark extends JsonSerializationBenchmark {
  @Benchmark
  def decodeCCCirce: Something =
    Something.ExampleJson.as[Something].fold(e => throw e, identity)

  @Benchmark
  def decodeCCGenCodec: Something =
    CirceJsonInput.read[Something](Something.ExampleJson)

  @Benchmark
  def decodeSHCirce: List[SealedStuff] =
    SealedStuff.ExampleJson.as[List[SealedStuff]].fold(e => throw e, identity)

  @Benchmark
  def decodeSHGenCodec: List[SealedStuff] =
    CirceJsonInput.read[List[SealedStuff]](SealedStuff.ExampleJson)

  @Benchmark
  def decodeFoosCirce: Map[String, Foo] =
    Foo.ExampleJson.as[Map[String, Foo]].fold(e => throw e, identity)

  @Benchmark
  def decodeFoosGenCodec: Map[String, Foo] =
    CirceJsonInput.read[Map[String, Foo]](Foo.ExampleJson)
}

class JsonWritingBenchmark extends JsonSerializationBenchmark {

  @Benchmark
  def writePrimitivesGenCodec: String =
    JsonStringOutput.write(Primitives.Example)

  @Benchmark
  def writeCCCirce: String =
    Something.Example.asJson.noSpaces

  @Benchmark
  def writeCCGenCodec: String =
    JsonStringOutput.write(Something.Example)

  @Benchmark
  def writeCCUpickle: String =
    upickle.default.write(Something.Example)

  @Benchmark
  def writeSHCirce: String =
    SealedStuff.ExampleList.asJson.noSpaces

  @Benchmark
  def writeSHGenCodec: String =
    JsonStringOutput.write(SealedStuff.ExampleList)

  @Benchmark
  def writeFlatSHGenCodec: String =
    JsonStringOutput.write(FlatSealedStuff.ExampleList)

  @Benchmark
  def writeSHUpickle: String =
    upickle.default.write(SealedStuff.ExampleList)

  @Benchmark
  def writeFoosCirce: String =
    Foo.ExampleMap.asJson.noSpaces

  @Benchmark
  def writeFoosGenCodec: String =
    JsonStringOutput.write(Foo.ExampleMap)

  @Benchmark
  def writeFoosUpickle: String =
    upickle.default.write(Foo.ExampleMap)
}

class JsonReadingBenchmark extends JsonSerializationBenchmark {
  @Benchmark
  def readCCCirce: Something =
    decode[Something](Something.ExampleJsonString).fold(e => throw e, identity)

  @Benchmark
  def readCCGenCodec: Something =
    JsonStringInput.read[Something](Something.ExampleJsonString)

  @Benchmark
  def readCCUpickle: Something =
    upickle.default.read[Something](Something.ExampleJsonString)

  @Benchmark
  def readSHCirce: List[SealedStuff] =
    decode[List[SealedStuff]](SealedStuff.ExampleJsonString).fold(e => throw e, identity)

  @Benchmark
  def readSHGenCodec: List[SealedStuff] =
    JsonStringInput.read[List[SealedStuff]](SealedStuff.ExampleJsonString)

  @Benchmark
  def readFlatSHGenCodec: List[FlatSealedStuff] =
    JsonStringInput.read[List[FlatSealedStuff]](FlatSealedStuff.ExampleJsonString)

  @Benchmark
  def readSHUpickle: List[SealedStuff] =
    upickle.default.read[List[SealedStuff]](SealedStuff.ExampleUpickleJsonString)

  @Benchmark
  def readFoosCirce: Map[String, Foo] =
    decode[Map[String, Foo]](Foo.ExampleJsonString).fold(e => throw e, identity)

  @Benchmark
  def readFoosGenCodec: Map[String, Foo] =
    JsonStringInput.read[Map[String, Foo]](Foo.ExampleJsonString)

  @Benchmark
  def readFoosUpickle: Map[String, Foo] =
    upickle.default.read[Map[String, Foo]](Foo.ExampleUpickleJsonString)
}

object JsonSerializationBenchmark {
  def main(args: Array[String]): Unit = {
    while (true) {
      JsonStringOutput.write[List[SealedStuff]](SealedStuff.ExampleList)
    }
  }
}
