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
  implicit val rw: upickle.default.ReadWriter[Something] = upickle.default.macroRW

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
  implicit val rw: upickle.default.ReadWriter[Stuff] = upickle.default.macroRW
}

sealed trait SealedStuff
case class Case1(i: Int) extends SealedStuff
object Case1 {
  implicit val rw: upickle.default.ReadWriter[Case1] = upickle.default.macroRW
}
case class Case2(i: Int) extends SealedStuff
object Case2 {
  implicit val rw: upickle.default.ReadWriter[Case2] = upickle.default.macroRW
}
case class Case3(i: Int) extends SealedStuff
object Case3 {
  implicit val rw: upickle.default.ReadWriter[Case3] = upickle.default.macroRW
}
case class Case4(i: Int) extends SealedStuff
object Case4 {
  implicit val rw: upickle.default.ReadWriter[Case4] = upickle.default.macroRW
}
case class Case5(i: Int) extends SealedStuff
object Case5 {
  implicit val rw: upickle.default.ReadWriter[Case5] = upickle.default.macroRW
}
case class Case6(i: Int) extends SealedStuff
object Case6 {
  implicit val rw: upickle.default.ReadWriter[Case6] = upickle.default.macroRW
}
case class Case7(i: Int) extends SealedStuff
object Case7 {
  implicit val rw: upickle.default.ReadWriter[Case7] = upickle.default.macroRW
}
object SealedStuff {
  implicit val codec: GenCodec[SealedStuff] = GenCodec.materialize
  implicit val encoder: Encoder[SealedStuff] = deriveEncoder[SealedStuff]
  implicit val decoder: Decoder[SealedStuff] = deriveDecoder[SealedStuff]
  implicit val rw: upickle.default.ReadWriter[SealedStuff] = upickle.default.macroRW

  final val ExampleList = List[SealedStuff](Case5(5), Case3(3), Case1(1), Case7(7), Case2(2), Case4(4), Case6(6))
  final val ExampleJson = ExampleList.asJson
  final val ExampleJsonString = ExampleJson.noSpaces
}

case class Foo(s: String, d: Double, i: Int, l: Long, bs: List[Boolean])
object Foo {
  implicit val circeEncodeFoo: Encoder[Foo] = deriveEncoder
  implicit val circeDecodeFoo: Decoder[Foo] = deriveDecoder
  implicit val codec: GenCodec[Foo] = GenCodec.materialize
  implicit val rw: upickle.default.ReadWriter[Foo] = upickle.default.macroRW

  final val ExampleMap: Map[String, Foo] = List.tabulate(100) { i =>
    ("b" * i) -> Foo("a" * i, (i + 2.0) / (i + 1.0), i, i * 1000L, (0 to i).map(_ % 2 == 0).toList)
  }.toMap

  final val ExampleJson = ExampleMap.asJson
  final val ExampleJsonString = ExampleJson.noSpaces
}

@Warmup(iterations = 10)
@Measurement(iterations = 20)
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

  //upickle cannot read example circe json strings
  //  @Benchmark
  //  def readSHUpickle: List[SealedStuff] =
  //    upickle.default.read[List[SealedStuff]](SealedStuff.ExampleJsonString)

  @Benchmark
  def readFoosCirce: Map[String, Foo] =
    decode[Map[String, Foo]](Foo.ExampleJsonString).fold(e => throw e, identity)

  @Benchmark
  def readFoosGenCodec: Map[String, Foo] =
    JsonStringInput.read[Map[String, Foo]](Foo.ExampleJsonString)

  //  @Benchmark
  //  def readFoosUpickle: Map[String, Foo] =
  //    upickle.default.read[Map[String, Foo]](Foo.ExampleJsonString)
}

object JsonSerializationBenchmark {
  def main(args: Array[String]): Unit = {
    while (true) {
      JsonStringOutput.write[List[SealedStuff]](SealedStuff.ExampleList)
    }
  }
}
