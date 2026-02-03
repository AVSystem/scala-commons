package com.avsystem.commons
package misc

import com.avsystem.commons.annotation.positioned
import com.avsystem.commons.meta.*
import com.avsystem.commons.serialization.*

abstract class HasGenCodecStructure[T](
  using macroInstances: MacroInstances[Unit, (codec: GenCodec[T], structure: GenStructure[T])],
) {
  given GenCodec[T] = macroInstances((), this).codec
  given GenStructure[T] = macroInstances((), this).structure
}

sealed trait GenStructure[T] extends TypedMetadata[T] {
  def repr: String
}
object GenStructure extends AdtMetadataCompanion[GenStructure] {
  given [T] => GenStructure[T] = ???
}

case class GenField[T](
  @infer ts: TypeString[T],
  @infer codec: GenCodec[T],
  @composite info: GenParamInfo[T],
) extends TypedMetadata[T] {
  def rawName: String = info.rawName
  def repr: String = s"[$info.flags]${info.annotName.fold("")(n => s"<${n.name}> ")}$ts"
}

@positioned(positioned.here) case class GenUnion[T](
  @composite info: GenUnionInfo[T],
  @multi @adtCaseMetadata cases: Map[String, GenCase[?]],
) extends GenStructure[T] {
  def repr: String = cases.iterator
    .map { case (name, gr) =>
      s"case $name:${gr.repr}"
    }
    .mkString(s"[${info.flags}]\n", "\n", "")
}

sealed trait GenCase[T] extends TypedMetadata[T] {
  def repr: String
  def info: GenCaseInfo[T]
  def sealedParents: List[GenSealedParent[?]]
}

case class GenSealedParent[T](
  @infer repr: TypeString[T],
) extends TypedMetadata[T]

@positioned(positioned.here) case class GenCustomCase[T](
  @composite info: GenCaseInfo[T],
  @multi @adtCaseSealedParentMetadata sealedParents: List[GenSealedParent[?]],
  @checked @infer structure: GenStructure.Lazy[T],
) extends GenCase[T] {
  def repr: String = structure.value.repr
}

@positioned(positioned.here) case class GenRecord[T](
  @composite info: GenCaseInfo[T],
  @multi @adtParamMetadata fields: Map[String, GenField[?]],
  @multi @adtCaseSealedParentMetadata sealedParents: List[GenSealedParent[?]],
) extends GenCase[T]
    with GenStructure[T] {

  def repr(indent: Int): String = fields.iterator
    .map { case (name, gf) =>
      s"${" " * indent}$name: ${gf.repr}"
    }
    .mkString(s"[${info.flags}]\n", "\n", "")

  def repr: String = repr(0)
}

@positioned(positioned.here) case class GenSingleton[T](
  @composite info: GenCaseInfo[T],
  @checked @infer valueOf: ValueOf[T],
  @multi @adtCaseSealedParentMetadata sealedParents: List[GenSealedParent[?]],
) extends GenCase[T]
    with GenStructure[T] {
  def repr: String = valueOf.value.toString
}

@allowUnorderedSubtypes
case class GenUnorderedUnion[T](
  @composite info: GenUnionInfo[T],
  @multi @adtCaseMetadata cases: Map[String, GenCase[?]],
) extends TypedMetadata[T]
object GenUnorderedUnion extends AdtMetadataCompanion[GenUnorderedUnion] {
  materialize[Option[String]]
}

sealed trait Being
object Being extends HasGenCodecStructure[Being]

sealed trait MaterialBeing extends Being

case class Person(name: String, @name("raw_age") age: Int) extends MaterialBeing
object Person extends HasGenCodecStructure[Person]

case class Galaxy(name: String, distance: Long) extends MaterialBeing

class Peculiarity extends Being
object Peculiarity {
  given GenCodec[Peculiarity] | Null = null
  given GenStructure[Peculiarity] | Null = null
}

case object God extends Being
