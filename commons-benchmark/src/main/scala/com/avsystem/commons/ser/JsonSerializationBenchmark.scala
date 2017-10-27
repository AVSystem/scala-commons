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

sealed trait SealedStuff
case class Case1(i: Int) extends SealedStuff
case class Case2(i: Int) extends SealedStuff
case class Case3(i: Int) extends SealedStuff
case class Case4(i: Int) extends SealedStuff
case class Case5(i: Int) extends SealedStuff
case class Case6(i: Int) extends SealedStuff
case class Case7(i: Int) extends SealedStuff
object SealedStuff {
  implicit val codec: GenCodec[SealedStuff] = GenCodec.materialize
  implicit val encoder: Encoder[SealedStuff] = deriveEncoder[SealedStuff]
  implicit val decoder: Decoder[SealedStuff] = deriveDecoder[SealedStuff]

  final val ExampleList = List[SealedStuff](Case5(5), Case3(3), Case1(1), Case7(7), Case2(2), Case4(4), Case6(6))
  final val ExampleJson = ExampleList.asJson
  final val ExampleJsonString = ExampleJson.noSpaces
}

@Warmup(iterations = 10)
@Measurement(iterations = 20)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class JsonSerializationBenchmark {
  @Benchmark
  def circeJsonWriting: Json =
    Something.Example.asJson

  @Benchmark
  def circeJsonReading: Something =
    Something.ExampleJson.as[Something].fold(e => throw e, identity)

  @Benchmark
  def circeJsonStringWriting: String =
    Something.Example.asJson.noSpaces

  @Benchmark
  def circeJsonStringReading: Something =
    decode[Something](Something.ExampleJsonString).fold(e => throw e, identity)

  @Benchmark
  def circeSealedJsonWriting: Json =
    SealedStuff.ExampleList.asJson

  @Benchmark
  def circeSealedJsonReading: List[SealedStuff] =
    SealedStuff.ExampleJson.as[List[SealedStuff]].fold(e => throw e, identity)

  @Benchmark
  def circeSealedJsonStringWriting: String =
    SealedStuff.ExampleList.asJson.noSpaces

  @Benchmark
  def circeSealedJsonStringReading: List[SealedStuff] =
    decode[List[SealedStuff]](SealedStuff.ExampleJsonString).fold(e => throw e, identity)

  @Benchmark
  def genCodecJsonWriting: Json =
    CirceJsonOutput.write(Something.Example)

  @Benchmark
  def genCodecJsonReading: Something =
    CirceJsonInput.read[Something](Something.ExampleJson)

  @Benchmark
  def genCodecJsonStringWriting: String =
    JsonStringOutput.write(Something.Example)

  @Benchmark
  def genCodecJsonStringReading: Something =
    JsonStringInput.read[Something](Something.ExampleJsonString)

  @Benchmark
  def genCodecSealedJsonWriting: Json =
    CirceJsonOutput.write(SealedStuff.ExampleList)

  @Benchmark
  def genCodecSealedJsonReading: List[SealedStuff] =
    CirceJsonInput.read[List[SealedStuff]](SealedStuff.ExampleJson)

  @Benchmark
  def genCodecSealedJsonStringWriting: String =
    JsonStringOutput.write(SealedStuff.ExampleList)

  @Benchmark
  def genCodecSealedJsonStringReading: List[SealedStuff] =
    JsonStringInput.read[List[SealedStuff]](SealedStuff.ExampleJsonString)
}

object JsonSerializationBenchmark {
  def main(args: Array[String]): Unit = {
    while (true) {
      JsonStringOutput.write[List[SealedStuff]](SealedStuff.ExampleList)
    }
  }
}
