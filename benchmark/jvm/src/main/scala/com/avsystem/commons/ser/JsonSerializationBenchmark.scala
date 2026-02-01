package com.avsystem.commons
package ser

import com.avsystem.commons.serialization.json.{JsonStringInput, JsonStringOutput}
import org.openjdk.jmh.annotations._

@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 2)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
abstract class JsonSerializationBenchmark

class JsonWritingBenchmark extends JsonSerializationBenchmark {
  @Benchmark
  def writePrimitivesGenCodec: String =
    JsonStringOutput.write(Primitives.Example)

  @Benchmark
  def writeCCGenCodec: String =
    JsonStringOutput.write(Something.Example)

  @Benchmark
  def writeSHGenCodec: String =
    JsonStringOutput.write(SealedStuff.ExampleList)

  @Benchmark
  def writeFlatSHGenCodec: String =
    JsonStringOutput.write(FlatSealedStuff.ExampleList)

  @Benchmark
  def writeFoosGenCodec: String =
    JsonStringOutput.write(Foo.ExampleMap)
}

class JsonReadingBenchmark extends JsonSerializationBenchmark {
  @Benchmark
  def readPrimitivesGenCodec: Primitives =
    JsonStringInput.read[Primitives](Primitives.ExampleJsonString)

  @Benchmark
  def readCCGenCodec: Something =
    JsonStringInput.read[Something](Something.ExampleJsonString)

  @Benchmark
  def readSHGenCodec: List[SealedStuff] =
    JsonStringInput.read[List[SealedStuff]](SealedStuff.ExampleJsonString)

  @Benchmark
  def readFlatSHGenCodec: List[FlatSealedStuff] =
    JsonStringInput.read[List[FlatSealedStuff]](FlatSealedStuff.ExampleJsonString)

  @Benchmark
  def readFoosGenCodec: Map[String, Foo] =
    JsonStringInput.read[Map[String, Foo]](Foo.ExampleJsonString)
}

object JsonSerializationBenchmark {
  def main(args: Array[String]): Unit = {
    while (true) {
      JsonStringOutput.write[List[SealedStuff]](SealedStuff.ExampleList)
    }
  }
}
