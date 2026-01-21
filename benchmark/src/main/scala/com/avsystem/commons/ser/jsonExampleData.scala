package com.avsystem.commons
package ser

import com.avsystem.commons.serialization.json.JsonStringOutput
import com.avsystem.commons.serialization.{HasGenCodec, flatten}

import scala.annotation.nowarn

case class Something(
  name: String,
  year: Int,
  stuffs: List[Stuff],
  ints: Set[Int]
)
@nowarn
object Something extends HasGenCodec[Something] {
  val Example: Something = Something(
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

  final val ExampleJsonString: String = JsonStringOutput.write(Example)
}

case class Primitives(
  b: Boolean,
  i: Int,
  l: Long,
  d: Double
)
@nowarn
object Primitives extends HasGenCodec[Primitives] {
  val Example: Primitives = Primitives(b = true, 42, 4332565, 3.14)

  final val ExampleJsonString: String = JsonStringOutput.write(Example)
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
  final val ExampleList: List[SealedStuff] = List[SealedStuff](Case5(5), Case3(3), Case1(1), Case7(7), Case2(2), Case4(4), Case6(6))
  final val ExampleJsonString: String = JsonStringOutput.write(ExampleList)
}
object FlatSealedStuff extends HasGenCodec[FlatSealedStuff] {

  final val ExampleList: List[FlatSealedStuff] = List[FlatSealedStuff](Case5(5), Case3(3), Case1(1), Case7(7), Case2(2), Case4(4), Case6(6))
  final val ExampleJsonString: String = JsonStringOutput.write(ExampleList)
}

case class Foo(s: String, d: Double, i: Int, l: Long, bs: List[Boolean])
@nowarn
object Foo extends HasGenCodec[Foo] {
  final val ExampleMap: Map[String, Foo] = List.tabulate(100) { i =>
    ("b" * i) -> Foo("a" * i, (i + 2.0) / (i + 1.0), i, i * 1000L, (0 to i).map(_ % 2 == 0).toList)
  }.toMap

  final val ExampleJsonString: String = JsonStringOutput.write(ExampleMap)
}