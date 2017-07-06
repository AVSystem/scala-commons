package com.avsystem.commons
package serialization

import org.scalatest.FunSuite

case class Bottom(mapa: Map[String, Int])
case class Middle(@name("bot") bottom: Bottom)
case class Toplevel(middle: Middle)

case class CodecRef[S, T](ref: GenRef[S, T])(implicit targetCodec: GenCodec[T])

class GenRefTest extends FunSuite {
  test("simple raw ref") {
    val path = RawRef[Toplevel](_.middle.bottom.mapa("str")).normalize.toList
    assert(path == List("middle", "bot", "mapa", "str").map(RawRef.Field))
  }

  test("simple gen ref") {
    val ref = GenRef[Toplevel](_.middle.bottom.mapa("str"))
    val obj = Toplevel(Middle(Bottom(Map("str" -> 42))))
    assert(ref(obj) == 42)
  }

  test("gen ref splicing") {
    val subRef = GenRef[Toplevel](_.middle)
    val ref1 = GenRef[Toplevel](t => subRef(t).bottom.mapa("str"))
    val ref2 = GenRef[Toplevel](subRef andThen (_.bottom.mapa("str")))
    val obj = Toplevel(Middle(Bottom(Map("str" -> 42))))
    assert(ref1(obj) == 42)
    assert(ref1.rawRef.normalize.toList == List("middle", "bot", "mapa", "str").map(RawRef.Field))
    assert(ref2(obj) == 42)
    assert(ref2.rawRef.normalize.toList == List("middle", "bot", "mapa", "str").map(RawRef.Field))
  }

  test("gen ref implicits test") {
    import GenRef.Implicits._

    val codecRef = CodecRef((_: Toplevel).middle.bottom.mapa("str"))
    val obj = Toplevel(Middle(Bottom(Map("str" -> 42))))
    assert(codecRef.ref(obj) == 42)
  }
}
