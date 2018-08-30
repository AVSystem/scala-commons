package com.avsystem.commons
package misc

import com.avsystem.commons.rpc._
import com.avsystem.commons.serialization.{GenCodec, defaultCase, flatten, name, transientDefault, transparent, whenAbsent}
import org.scalatest.FunSuite

abstract class HasGenCodecStructure[T](implicit gc: MacroGenerated[GenCodec[T]], gs: MacroGenerated[GenStructure[T]]) {
  implicit val genCodec: GenCodec[T] = gc.forCompanion(this)
  implicit val genStructure: GenStructure[T] = gs.forCompanion(this)
}

sealed trait GenStructure[T] extends TypedMetadata[T] {
  def repr: String
}
object GenStructure extends AdtMetadataCompanion[GenStructure]

case class GenField[T](
  @infer ts: TypeString[T],
  @infer codec: GenCodec[T],
  @reifyName sourceName: String,
  @optional @reifyAnnot annotName: Opt[name],
  @optional @reifyAnnot whenAbsent: Opt[whenAbsent[T]],
  @isAnnotated[transientDefault] transientDefault: Boolean,
  @reifyFlags flags: ParamFlags
) extends TypedMetadata[T] {
  val hasFallbackValue: Boolean = whenAbsent.fold(flags.hasDefaultValue)(wa => Try(wa.value).isSuccess)
  def rawName: String = annotName.fold(sourceName)(_.name)
  def repr: String = s"${annotName.fold("")(n => s"<${n.name}> ")}$ts"
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

sealed trait GenCase[T] extends GenStructure[T] {
  def defaultCase: Boolean
  def sourceName: String
  def annotName: Opt[name]
  def rawName: String = annotName.fold(sourceName)(_.name)
}

case class GenRecord[T](
  @multi @adtParamMetadata fields: Map[String, GenField[_]],
  @reifyName sourceName: String,
  @optional @reifyAnnot annotName: Opt[name],
  @optional @reifyAnnot flatten: Opt[flatten],
  @isAnnotated[transparent] transparent: Boolean,
  @isAnnotated[defaultCase] defaultCase: Boolean
) extends GenCase[T] {

  def repr(indent: Int): String = fields.iterator.map {
    case (name, gf) => s"${" " * indent}$name: ${gf.repr}"
  }.mkString("\n")

  def repr: String = repr(0)
}

case class GenSingleton[T](
  @infer @checked valueOf: ValueOf[T],
  @reifyName sourceName: String,
  @optional @reifyAnnot annotName: Opt[name],
  @isAnnotated[defaultCase] defaultCase: Boolean
) extends GenCase[T] {
  def repr: String = valueOf.value.toString
}

sealed trait Being
object Being extends HasGenCodecStructure[Being]

case class Person(name: String, @name("raw_age") age: Int) extends Being
object Person extends HasGenCodecStructure[Person]

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
