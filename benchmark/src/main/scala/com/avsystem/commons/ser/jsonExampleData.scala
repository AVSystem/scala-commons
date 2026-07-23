package com.avsystem.commons
package ser

import com.avsystem.commons.serialization.json.JsonStringOutput
import com.avsystem.commons.serialization.{flatten, HasGenCodec}

import scala.annotation.nowarn

case class Something(
  name: String,
  year: Int,
  stuffs: List[Stuff],
  ints: Set[Int],
)
@nowarn
object Something extends HasGenCodec[Something] {
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
      1 // 5, 62, -23, 454, 123, 75, -234,
    ),
  )

  final val ExampleJsonString = JsonStringOutput.write(Example)
}

case class Primitives(
  b: Boolean,
  i: Int,
  l: Long,
  d: Double,
)
@nowarn
object Primitives extends HasGenCodec[Primitives] {
  val Example = Primitives(b = true, 42, 4332565, 3.14)

  final val ExampleJsonString = JsonStringOutput.write(Example)
}

case class Stuff(map: Map[String, Boolean], factor: Double)
@nowarn
object Stuff extends HasGenCodec[Stuff]

@flatten sealed trait FlatSealedStuff
sealed trait SealedStuff
case class Case1(i: Int) extends SealedStuff with FlatSealedStuff
case class Case2(i: Int) extends SealedStuff with FlatSealedStuff
case class Case3(i: Int) extends SealedStuff with FlatSealedStuff
case class Case4(i: Int) extends SealedStuff with FlatSealedStuff
case class Case5(i: Int) extends SealedStuff with FlatSealedStuff
case class Case6(i: Int) extends SealedStuff with FlatSealedStuff
case class Case7(i: Int) extends SealedStuff with FlatSealedStuff
@nowarn
object SealedStuff extends HasGenCodec[SealedStuff] {
  final val ExampleList = List[SealedStuff](Case5(5), Case3(3), Case1(1), Case7(7), Case2(2), Case4(4), Case6(6))
  final val ExampleJsonString = JsonStringOutput.write(ExampleList)
}
object FlatSealedStuff extends HasGenCodec[FlatSealedStuff] {

  final val ExampleList = List[FlatSealedStuff](Case5(5), Case3(3), Case1(1), Case7(7), Case2(2), Case4(4), Case6(6))
  final val ExampleJsonString = JsonStringOutput.write(ExampleList)
}

object BigDoubleMap {
  // Large String -> Double map, with a spread of double shapes: small fractions, large/small magnitudes,
  // negatives and whole numbers. Used to compare Double-formatting strategies.
  final val Example: Map[String, Double] = List
    .tabulate(128) { i =>
      val d = ((i + 1) * 0.123456789) * (if (i % 2 == 0) 1 else -1) * math.pow(10, (i % 7) - 3)
      s"field_$i" -> d
    }
    .toMap

  final val ExampleJsonStandard: String = JsonStringOutput.write(Example)
}

/** A large JSON object mixing all four number types the fast codec covers (`Int`, `Long`, `Double`, `Float`), spread
  * across four maps of 32 entries each — 132 keys total. Represents a realistic numeric-heavy payload (e.g. a Kafka
  * telemetry record) and is the end-to-end target for comparing [[JsonNumberCodec.Standard]] vs
  * [[JsonNumberCodec.Fast]] on both serialization and deserialization.
  */
case class BigNumbers(
  ints: Map[String, Int],
  longs: Map[String, Long],
  doubles: Map[String, Double],
  floats: Map[String, Float],
)
@nowarn
object BigNumbers extends HasGenCodec[BigNumbers] {
  private def tab[V](f: Int => V): Map[String, V] =
    List.tabulate(32)(i => s"field_$i" -> f(i)).toMap

  final val Example: BigNumbers = BigNumbers(
    ints = tab(i => (i * 2654435761L - 1234567890L).toInt),
    longs = tab(i => i.toLong * 2654435761L - 1234567890123L),
    doubles = tab(i => ((i + 1) * 0.123456789) * (if (i % 2 == 0) 1 else -1) * math.pow(10, (i % 7) - 3)),
    floats = tab(i => (((i + 1) * 0.31830988f) * (if (i % 2 == 0) 1 else -1) * math.pow(10, (i % 5) - 2)).toFloat),
  )

  final val ExampleJsonStandard: String = JsonStringOutput.write(Example)
}

case class Foo(s: String, d: Double, i: Int, l: Long, bs: List[Boolean])
@nowarn
object Foo extends HasGenCodec[Foo] {
  final val ExampleMap: Map[String, Foo] = List
    .tabulate(100) { i =>
      ("b" * i) -> Foo("a" * i, (i + 2.0) / (i + 1.0), i, i * 1000L, (0 to i).map(_ % 2 == 0).toList)
    }
    .toMap

  final val ExampleJsonString = JsonStringOutput.write(ExampleMap)
}
