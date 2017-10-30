package com.avsystem.commons
package ser

import com.avsystem.commons.serialization.GenCodec
import com.avsystem.commons.serialization.json.{JsonStringInput, JsonStringOutput}
import io.circe._
import io.circe.generic.semiauto._
import io.circe.parser._
import io.circe.syntax._
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, Warmup}

case class Something(
  name: String,
  year: Int,
  stuffs: List[Stuff],
  ints: Set[Int]
)
object Something {
  implicit val codec: GenCodec[Something] = GenCodec.materialize
  implicit val encoder: Encoder[Something] = deriveEncoder[Something]
  implicit val decoder: Decoder[Something] = deriveDecoder[Something]

  val Example = Something(
    "The Name of Something",
    2017,
    List(
      Stuff(Map(), 3.15)
//      Stuff(Map("fuu" -> true, "boo" -> false, "fag" -> true), 3.14),
//      Stuff(Map("fuu" -> true), 3.16),
//      Stuff(Map("fuu" -> true, "boo \n\r\t" -> false, "fag" -> true, "moar" -> false), 3.17),
//      Stuff(Map.empty, 3.18),
//      Stuff(Map("fuu" -> true, "boo" -> false, "fag" -> true), 3.19),
    ),
    Set(
      1 //5, 62, -23, 454, 123, 75, -234,
    )
  )

  final val ExampleJson = Example.asJson
  final val ExampleJsonString = ExampleJson.noSpaces
}

case class Stuff(map: Map[String, Boolean], factor: Double)
object Stuff {
  implicit val codec: GenCodec[Stuff] = GenCodec.materialize
  implicit val encoder: Encoder[Stuff] = deriveEncoder[Stuff]
  implicit val decoder: Decoder[Stuff] = deriveDecoder[Stuff]
}

@Warmup(iterations = 10)
@Measurement(iterations = 20)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class JsonSerializationBenchmark {
  @Benchmark
  def genCodecJsonStringWriting: String = JsonStringOutput.write(Something.Example)

  @Benchmark
  def genCodecJsonStringReading: Something = JsonStringInput.read[Something](Something.ExampleJsonString)

  @Benchmark
  def circeJsonStringWriting: String = Something.Example.asJson.noSpaces

  @Benchmark
  def circeJsonStringReading: Something = decode[Something](Something.ExampleJsonString).fold(e => throw e, identity)

  @Benchmark
  def genCodecJsonWriting: Json = CirceJsonOutput.write(Something.Example)

  @Benchmark
  def genCodecJsonReading: Something = CirceJsonInput.read[Something](Something.ExampleJson)

  @Benchmark
  def circeJsonWriting: Json = Something.Example.asJson

  @Benchmark
  def circeJsonReading: Something = Something.ExampleJson.as[Something].fold(e => throw e, identity)
}

object JsonSerializationBenchmark {
  def main(args: Array[String]): Unit = {
    println(JsonStringOutput.write(Something.Example) == Something.ExampleJsonString)
    println(JsonStringInput.read[Something](Something.ExampleJsonString) == Something.Example)
    println(CirceJsonOutput.write(Something.Example) == Something.ExampleJson)
    println(CirceJsonInput.read[Something](Something.ExampleJson) == Something.Example)

    while(true) {
      CirceJsonInput.read[Something](Something.ExampleJson)
    }
  }
}
