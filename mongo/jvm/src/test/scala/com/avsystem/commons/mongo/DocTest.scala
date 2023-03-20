package com.avsystem.commons
package mongo

import org.bson.BsonString
import org.scalatest.funsuite.AnyFunSuite

/**
  * @author MKej
  */
class DocTest extends AnyFunSuite {

  import DocTest._

  test("get variance") {
    val doc = Doc()
    doc.get(something): Option[Something]
    doc.get(somethingCool): Option[SomethingCool.type]
    doc.get(somethingCool): Option[Something]
    doc.get(stringSet): Option[BSet[String]]

    assertDoesNotCompile("doc.get(something): Option[SomethingCool.type]")
    assertDoesNotCompile("doc.get(something): Option[SomethingShiny.type]")
    assertDoesNotCompile("doc.get(stringSet): Option[ISet[String]")
    assertDoesNotCompile("doc.get(stringSet): Option[MSet[String]")
  }

  test("put variance") {
    val doc = Doc()
    doc
      .put(something, SomethingShiny)
      .put(somethingCool, SomethingCool)
      .put(stringSet, ISet("one", "two", "three"))
      .put(stringSet, MSet("one", "two", "three"))

    assertDoesNotCompile("doc.put(somethingCool, SomethingShiny)")
  }
}

object DocTest {
  sealed trait Something
  object SomethingCool extends Something
  object SomethingShiny extends Something

  val Cool = "cool"
  val Shiny = "shiny"

  val something = new BsonCodec[Something, BsonString] {
    override def fromBson(bson: BsonString) = bson.getValue match {
      case Cool => SomethingCool
      case Shiny => SomethingShiny
    }
    override def toBson(a: Something) = new BsonString(a match {
      case SomethingCool => Cool
      case SomethingShiny => Shiny
    })
  }.key("something")

  val somethingCool = new BsonCodec[SomethingCool.type, BsonString] {
    override def fromBson(bson: BsonString) = SomethingCool
    override def toBson(a: SomethingCool.type) = new BsonString(Cool)
  }.key("somethingCool")

  val stringSet = BsonCodec.string.collection[BSet].key("stringSet")
}
