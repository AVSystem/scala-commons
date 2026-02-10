package com.avsystem.commons
package serialization

import com.avsystem.commons.mirror.{name, transparent}
import org.scalatest.funsuite.AnyFunSuite

@flatten sealed trait Seal {
  def id: String
}
case class Klass(@name("_id") id: String) extends Seal
case object Objekt extends Seal {
  @generated
  @name("_id") def id: String = "O"
}

case class Bottom(mapa: Map[String, Int], wrappy: Wrappy)
case class Middle(@name("bot") bottom: Bottom)
case class Toplevel(middle: Opt[Middle], seal: Seal = Objekt)
@transparent case class TransparentToplevel(toplevel: Toplevel)

case class Wrappy(value: String) extends AnyVal
//object Wrappy extends StringWrapperCompanion[Wrappy]

case class CodecRef[S, T](ref: GenRef[S, T])(using targetCodec: GenCodec[T])

@flatten sealed trait GenericUnion[T] {
  def thing: T
}
case class GenericCase[T](thing: T) extends GenericUnion[T]

class GenRefTest extends AnyFunSuite {
//  test("simple raw ref") {
//    val path = RawRef.create[TransparentToplevel].ref(_.toplevel.middle.get.bottom.mapa("str")).normalize.toList
//    assert(path == List("middle", "bot", "mapa", "str").map(RawRef.Field.apply))
//  }
//
//  test("transparent wrapper field ref") {
//    val path1 = RawRef.create[Toplevel].ref(_.middle.get.bottom.wrappy).normalize.toList
//    val path2 = RawRef.create[Toplevel].ref(_.middle.get.bottom.wrappy.value).normalize.toList
//    assert(path1 == List("middle", "bot", "wrappy").map(RawRef.Field.apply))
//    assert(path2 == path1)
//  }
//
//  test("simple gen ref") {
//    val ref = GenRef.create[TransparentToplevel].ref(_.toplevel.middle.get.bottom.mapa("str"))
//    val obj = TransparentToplevel(Toplevel(Middle(Bottom(Map("str" -> 42), Wrappy("oof"))).opt))
//    assert(ref(obj) == 42)
//  }
//
//  test("gen ref composition") {
//    val subRef = GenRef.create[TransparentToplevel].ref(_.toplevel.middle.get)
//    val ref1 = subRef.andThen(GenRef.create[Middle].ref(_.bottom.mapa("str")))
//    val ref2 = subRef.andThen(GenRef.create[Middle].ref(_.bottom.mapa("str")))
//    val obj = TransparentToplevel(Toplevel(Middle(Bottom(Map("str" -> 42), Wrappy("oof"))).opt))
//    assert(ref1(obj) == 42)
//    assert(ref1.rawRef.normalize.toList == List("middle", "bot", "mapa", "str").map(RawRef.Field.apply))
//    assert(ref2(obj) == 42)
//    assert(ref2.rawRef.normalize.toList == List("middle", "bot", "mapa", "str").map(RawRef.Field.apply))
//  }
//
//  test("gen ref implicits test") {
//    import GenRef.Implicits.*
//
//    val codecRef = CodecRef((_: TransparentToplevel).toplevel.middle.get.bottom.mapa("str"))
//    val obj = TransparentToplevel(Toplevel(Middle(Bottom(Map("str" -> 42), Wrappy("oof"))).opt))
//    assert(codecRef.ref(obj) == 42)
//  }
//
//  test("sealed trait common field test") {
//    val ref = GenRef.create[Toplevel].ref(_.seal.id)
//    assert(ref.rawRef.normalize.toList == List("seal", "_id").map(RawRef.Field.apply))
//  }
//
//  test("generic sealed trait common field test") {
//    val ref = GenRef.create[GenericUnion[Int]].ref(_.thing)
//    assert(ref.rawRef.normalize.toList == List(RawRef.Field("thing")))
//  }
}
