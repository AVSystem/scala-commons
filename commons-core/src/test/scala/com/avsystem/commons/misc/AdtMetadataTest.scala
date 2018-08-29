package com.avsystem.commons
package misc

import com.avsystem.commons.rpc._
import com.avsystem.commons.serialization.{defaultCase, flatten, name, transparent}
import org.scalatest.FunSuite

abstract class HasGenStructure[T](implicit gs: MacroGenerated[GenStructure[T]]) {
  implicit val genStructure: GenStructure[T] = gs.forCompanion(this)
}

sealed trait GenStructure[T] extends TypedMetadata[T] {
  def repr: String
}
object GenStructure extends AdtMetadataCompanion[GenStructure]

case class GenField[T](
  @infer ts: TypeString[T],
  @optional @reifyAnnot rawName: Opt[name]
) extends TypedMetadata[T] {

  def repr: String = s"${rawName.fold("")(n => s"<${n.name}> ")}$ts"
}

case class GenUnion[T](
  @multi @adtCaseMetadata classes: Map[String, GenRecord[_]],
  @multi @adtCaseMetadata objects: Map[String, GenSingleton[_]],
) extends GenStructure[T] {

  def repr: String = {
    val forClasses = classes.iterator.map {
      case (name, gr) => s"case $name:\n${gr.repr(2)}"
    }
    val forObjects = objects.iterator.map {
      case (name, _) => s"case $name"
    }
    (forClasses ++ forObjects).mkString("\n")
  }
}

case class GenRecord[T](
  @multi @adtParamMetadata fields: Map[String, GenField[_]],
  @optional @reifyAnnot rawName: Opt[name],
  @optional @reifyAnnot flatten: Opt[flatten],
  @optional @isAnnotated[transparent] transparent: Boolean,
  @optional @isAnnotated[defaultCase] defaultCase: Boolean
) extends GenStructure[T] {

  def repr(indent: Int): String = fields.iterator.map {
    case (name, gf) => s"${" " * indent}$name: ${gf.repr}"
  }.mkString("\n")

  def repr: String = repr(0)
}

case class GenSingleton[T](
  @infer @checked valueOf: ValueOf[T],
  @optional @reifyAnnot rawName: Opt[name],
  @optional @isAnnotated[defaultCase] defaultCase: Boolean
) extends GenStructure[T] {

  def repr: String = valueOf.value.toString
}

sealed trait Being
object Being extends HasGenStructure[Being]

case class Person(name: String, @name("raw_age") age: Int) extends Being
object Person extends HasGenStructure[Person]

case object God extends Being

class AdtMetadataTest extends FunSuite {
  test("simple case class") {
    assert(Person.genStructure.repr ==
      """name: String
        |age: <raw_age> Int""".stripMargin
    )
  }

  test("simple sealed hierarchy") {
    assert(Being.genStructure.repr ==
      """case Person:
        |  name: String
        |  age: <raw_age> Int
        |case God""".stripMargin
    )
  }
}
