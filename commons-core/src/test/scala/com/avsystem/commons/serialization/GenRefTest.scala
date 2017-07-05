package com.avsystem.commons
package serialization

import org.scalatest.FunSuite

case class Bottom(str: String)
case class Middle(@name("bot") bottom: Bottom)
case class Toplevel(middle: Middle)

case class CodecRef[S, T](ref: GenRef[S, T])(implicit targetCodec: GenCodec[T])

class GenRefTest extends FunSuite {
  test("simple raw ref") {
    val path = RawRef[Toplevel](_.middle.bottom.str).normalize.toList
    assert(path == List("middle", "bot", "str").map(RawRef.Field))
  }

  test("simple gen ref") {
    val ref = GenRef[Toplevel](_.middle.bottom.str)
    val obj = Toplevel(Middle(Bottom("lol")))
    assert(ref(obj) == "lol")
  }

  test("gen ref implicits test") {
    import GenRef.Implicits._

    val codecRef = CodecRef((_: Toplevel).middle.bottom.str)
    val obj = Toplevel(Middle(Bottom("lol")))
    assert(codecRef.ref(obj) == "lol")
  }
}
