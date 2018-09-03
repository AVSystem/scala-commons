package com.avsystem.commons
package misc

import com.avsystem.commons.meta._
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
  def repr: String = s"[$flags]${annotName.fold("")(n => s"<${n.name}> ")}$ts"
}

case class GenUnion[T](
  @reifyFlags flags: TypeFlags,
  @multi @adtCaseMetadata cases: Map[String, GenCase[_]]
) extends GenStructure[T] {
  def repr: String = cases.iterator.map {
    case (name, gr) => s"case $name:${gr.repr(2)}"
  }.mkString(s"[$flags]\n", "\n", "")
}

case class GenCase[T](
  @reifyFlags flags: TypeFlags,
  @multi @adtParamMetadata fields: Map[String, GenField[_]],
  @reifyName sourceName: String,
  @optional @reifyAnnot annotName: Opt[name],
  @optional @reifyAnnot flatten: Opt[flatten],
  @isAnnotated[transparent] transparent: Boolean,
  @isAnnotated[defaultCase] defaultCase: Boolean
) extends GenStructure[T] {

  def repr(indent: Int): String = fields.iterator.map {
    case (name, gf) => s"${" " * indent}$name: ${gf.repr}"
  }.mkString(s"[$flags]\n", "\n", "")

  def repr: String = repr(0)
}

sealed trait Being
object Being extends HasGenCodecStructure[Being]

case class Person(name: String, @name("raw_age") age: Int) extends Being
object Person extends HasGenCodecStructure[Person]

case object God extends Being

class AdtMetadataTest extends FunSuite {
  test("simple case class") {
    println(Person.genStructure.repr)
    assert(Person.genStructure.repr ==
      """[case]
        |name: []String
        |age: []<raw_age> Int""".stripMargin
    )
  }

  test("simple sealed hierarchy") {
    println(Being.genStructure.repr)
    assert(Being.genStructure.repr ==
      """[abstract,sealed,trait]
        |case Person:[case]
        |  name: []String
        |  age: []<raw_age> Int
        |case God:[case,object]
        |""".stripMargin
    )
  }
}
