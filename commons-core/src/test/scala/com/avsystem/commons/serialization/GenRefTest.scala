package com.avsystem.commons
package serialization

import org.scalatest.FunSuite

import scala.annotation.unused

@flatten sealed trait Seal {
  def id: String
}
case class Klass(@name("_id") id: String) extends Seal
case object Objekt extends Seal {
  @generated
  @name("_id") def id: String = "O"
}

case class Bottom(mapa: Map[String, Int])
case class Middle(@name("bot") bottom: Bottom)
case class Toplevel(middle: Opt[Middle], seal: Seal = Objekt)
@transparent case class TransparentToplevel(toplevel: Toplevel)

case class CodecRef[S, T](ref: GenRef[S, T])(implicit @unused targetCodec: GenCodec[T])

class GenRefTest extends FunSuite {
  test("simple raw ref") {
    val path = RawRef.create[TransparentToplevel].ref(_.toplevel.middle.get.bottom.mapa("str")).normalize.toList
    assert(path == List("middle", "bot", "mapa", "str").map(RawRef.Field))
  }

  test("simple gen ref") {
    val ref = GenRef.create[TransparentToplevel].ref(_.toplevel.middle.get.bottom.mapa("str"))
    val obj = TransparentToplevel(Toplevel(Middle(Bottom(Map("str" -> 42))).opt))
    assert(ref(obj) == 42)
  }

  test("gen ref composition") {
    val subRef = GenRef.create[TransparentToplevel].ref(_.toplevel.middle.get)
    val ref1 = subRef andThen GenRef.create[Middle].ref(_.bottom.mapa("str"))
    val ref2 = subRef andThen GenRef.create[Middle].ref(_.bottom.mapa("str"))
    val obj = TransparentToplevel(Toplevel(Middle(Bottom(Map("str" -> 42))).opt))
    assert(ref1(obj) == 42)
    assert(ref1.rawRef.normalize.toList == List("middle", "bot", "mapa", "str").map(RawRef.Field))
    assert(ref2(obj) == 42)
    assert(ref2.rawRef.normalize.toList == List("middle", "bot", "mapa", "str").map(RawRef.Field))
  }

  test("gen ref implicits test") {
    import GenRef.Implicits._

    val codecRef = CodecRef((_: TransparentToplevel).toplevel.middle.get.bottom.mapa("str"))
    val obj = TransparentToplevel(Toplevel(Middle(Bottom(Map("str" -> 42))).opt))
    assert(codecRef.ref(obj) == 42)
  }

  test("sealed trait common field test") {
    val ref = GenRef.create[Toplevel].ref(_.seal.id)
    assert(ref.rawRef.normalize.toList == List("seal", "_id").map(RawRef.Field))
  }
}
