package com.avsystem.commons
package rpc

import com.avsystem.commons.meta.*
import com.avsystem.commons.misc.TypeString
import org.scalatest.funsuite.AnyFunSuite

class GenericMeta[T](
  @reifyName name: String,
  @multi @rpcTypeParamMetadata typeParams: List[GenericMeta.TypeParam],
  @multi @rpcMethodMetadata methods: List[GenericMeta.Method[?]],
) {
  def repr: String = {
    val targs = typeParams.map(_.typeString)
    s"$name${GenericMeta.tparams(typeParams)} {\n  ${methods.iterator.map(_.repr(targs)).mkString("\n  ")}\n}"
  }
}
object GenericMeta extends RpcMetadataCompanion[GenericMeta] {
  case class TypeParam(@reifyName name: String) {
    def typeString: TypeString[?] = new TypeString(name)
  }

  case class Param[T](
    @reifyName name: String,
    @infer @forTypeParams tpe: List[TypeString[?]] => TypeString[T],
  ) extends TypedMetadata[T] {
    def repr(targs: List[TypeString[?]]): String =
      s"$name: ${tpe(targs)}"
  }

  case class Method[T](
    @reifyName name: String,
    @multi @rpcTypeParamMetadata typeParams: List[TypeParam],
    @multi @rpcParamMetadata params: List[Param[?]],
    @infer @forTypeParams result: List[TypeString[?]] => TypeString[T],
  ) extends TypedMetadata[T] {
    def repr(targs: List[TypeString[?]]): String = {
      val fullTargs = targs ++ typeParams.map(_.typeString)
      val paramsRepr = params.iterator.map(_.repr(fullTargs)).mkStringOrEmpty("(", ", ", ")")
      s"$name${tparams(typeParams)}$paramsRepr: ${result(fullTargs)}"
    }
  }

  def tparams(typeParams: List[TypeParam]): String =
    typeParams.iterator.map(_.name).mkStringOrEmpty("[", ", ", "]")
}

trait GenericTrait[A, B] {
  def method(a: A, int: Int): List[A]
  def genericMethod[C](map: Map[A, C]): Map[B, C]
}
object GenericTrait {
  implicit val meta: GenericMeta[GenericTrait[?, ?]] = GenericMeta.materialize
}

class GenericMetadataTest extends AnyFunSuite {
  test("generic metadata") {
    assert(GenericTrait.meta.repr == """GenericTrait[A, B] {
                                       |  method(a: A, int: Int): List[A]
                                       |  genericMethod[C](map: Map[A, C]): Map[B, C]
                                       |}""".stripMargin)
  }
}
