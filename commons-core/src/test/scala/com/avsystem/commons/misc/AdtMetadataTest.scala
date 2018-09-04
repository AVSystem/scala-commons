package com.avsystem.commons
package misc

import com.avsystem.commons.meta._
import com.avsystem.commons.serialization.{GenCaseInfo, GenCodec, GenFieldInfo, GenUnionInfo, name}

abstract class HasGenCodecStructure[T](
  implicit gc: MacroGenerated[GenCodec[T]], gs: MacroGenerated[GenStructure[T]]) {
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
  @composite info: GenFieldInfo[T]
) extends TypedMetadata[T] {
  def rawName: String = info.rawName
  def repr: String = s"[$info.flags]${info.annotName.fold("")(n => s"<${n.name}> ")}$ts"
}

case class GenUnion[T](
  @composite info: GenUnionInfo[T],
  @multi @adtCaseMetadata cases: Map[String, GenCase[_]]
) extends GenStructure[T] {
  def repr: String = cases.iterator.map {
    case (name, gr) => s"case $name:${gr.repr}"
  }.mkString(s"[${info.flags}]\n", "\n", "")
}

sealed trait GenCase[T] extends TypedMetadata[T] {
  def repr: String
  def info: GenCaseInfo[T]
}

case class GenCustomCase[T](
  @composite info: GenCaseInfo[T],
  @checked @infer structure: GenStructure[T]
) extends GenCase[T] {
  def repr: String = structure.repr
}

case class GenRecord[T](
  @composite info: GenCaseInfo[T],
  @multi @adtParamMetadata fields: Map[String, GenField[_]],
) extends GenCase[T] with GenStructure[T] {

  def repr(indent: Int): String = fields.iterator.map {
    case (name, gf) => s"${" " * indent}$name: ${gf.repr}"
  }.mkString(s"[${info.flags}]\n", "\n", "")

  def repr: String = repr(0)
}

case class GenSingleton[T](
  @composite info: GenCaseInfo[T],
  @checked @infer valueOf: ValueOf[T]
) extends GenCase[T] with GenStructure[T] {
  def repr: String = valueOf.value.toString
}

sealed trait Being
object Being extends HasGenCodecStructure[Being]

case class Person(name: String, @name("raw_age") age: Int) extends Being
object Person extends HasGenCodecStructure[Person]

case class Galaxy(name: String, distance: Long) extends Being

class Peculiarity extends Being
object Peculiarity {
  implicit val codec: GenCodec[Peculiarity] = null
  implicit val structure: GenStructure[Peculiarity] = null
}

case object God extends Being
