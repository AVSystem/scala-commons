package com.avsystem.commons
package misc

import com.avsystem.commons.rpc.{TypedMetadata, adtParamMetadata, infer, multi, optional, reifyAnnot}
import com.avsystem.commons.serialization.name
import org.scalatest.FunSuite

case class GenField[T](@infer ts: TypeString[T], @optional @reifyAnnot rawName: Opt[name])
  extends TypedMetadata[T] {

  override def toString: String = s"${rawName.fold("")(n => s"<${n.name}> ")}$ts"
}

case class GenStructure[T](@multi @adtParamMetadata fields: Map[String, GenField[_]])
  extends TypedMetadata[T] {

  override def toString: String = fields.iterator.map {
    case (name, gf) => s"$name: $gf"
  }.mkString("\n")
}

object GenStructure extends AdtMetadataCompanion[GenStructure]

case class Person(name: String, @name("raw_age") age: Int)
object Person {
  implicit val structure: GenStructure[Person] = GenStructure.materialize
}

class AdtMetadataTest extends FunSuite {
  test("simple") {
    assert(Person.structure.toString ==
      """name: String
        |age: <raw_age> Int""".stripMargin
    )
  }
}
