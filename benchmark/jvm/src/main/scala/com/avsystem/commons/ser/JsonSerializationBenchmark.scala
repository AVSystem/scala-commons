package com.avsystem.commons
package ser

import com.avsystem.commons.serialization.json.{JsonNumberCodec, JsonOptions, JsonStringInput, JsonStringOutput}
import org.openjdk.jmh.annotations.*

@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 2)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
abstract class JsonSerializationBenchmark

/** End-to-end GenCodec serialization and deserialization comparing [[JsonNumberCodec.Standard]] vs
  * [[JsonNumberCodec.Fast]] on numeric-heavy payloads: a 128-entry `Map[String, Double]` ([[BigDoubleMap]]) and a
  * 132-key object mixing `Int`/`Long`/`Double`/`Float` ([[BigNumbers]]). Run with `-prof gc` to see the allocation
  * difference (Fast reads parse straight from the input buffer, avoiding a per-number substring).
  */
class JsonNumberCodecBenchmark extends JsonSerializationBenchmark {
  import JsonNumberCodecBenchmark.*

  // --- write (serialization) ---

  @Benchmark
  def writeDoubleMapStandard: String =
    JsonStringOutput.write(BigDoubleMap.Example, Standard)

  @Benchmark
  def writeDoubleMapFast: String =
    JsonStringOutput.write(BigDoubleMap.Example, Fast)

  @Benchmark
  def writeMixedStandard: String =
    JsonStringOutput.write(BigNumbers.Example, Standard)

  @Benchmark
  def writeMixedFast: String =
    JsonStringOutput.write(BigNumbers.Example, Fast)

  // --- read (deserialization) ---

  @Benchmark
  def readDoubleMapStandard: Map[String, Double] =
    JsonStringInput.read[Map[String, Double]](BigDoubleMap.ExampleJsonStandard, Standard)

  @Benchmark
  def readDoubleMapFast: Map[String, Double] =
    JsonStringInput.read[Map[String, Double]](BigDoubleMap.ExampleJsonStandard, Fast)

  @Benchmark
  def readMixedStandard: BigNumbers =
    JsonStringInput.read[BigNumbers](BigNumbers.ExampleJsonStandard, Standard)

  @Benchmark
  def readMixedFast: BigNumbers =
    JsonStringInput.read[BigNumbers](BigNumbers.ExampleJsonStandard, Fast)
}

object JsonNumberCodecBenchmark {
  final val Standard: JsonOptions = JsonOptions.Default
  final val Fast: JsonOptions = JsonOptions(numberCodec = JsonNumberCodec.Fast)
}

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
