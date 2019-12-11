package com.avsystem.commons
package rpc

import com.avsystem.commons.meta._
import com.avsystem.commons.misc.TypeString
import com.avsystem.commons.rpc.GenericMeta._
import org.scalatest.FunSuite

class GenericMeta[T](
  @reifyName name: String,
  @multi @rpcTypeParamMetadata typeParams: List[TypeParam],
  @multi @rpcMethodMetadata methods: List[Method[_]]
) {
  def repr: String = {
    val targs = typeParams.map(_.typeString)
    s"$name${tparams(typeParams)} {\n  ${methods.iterator.map(_.repr(targs)).mkString("\n  ")}\n}"
  }
}
object GenericMeta extends RpcMetadataCompanion[GenericMeta] {
  case class TypeParam(@reifyName name: String) {
    def typeString: TypeString[_] = new TypeString(name)
  }

  case class Param[T](
    @reifyName name: String,
    @infer @forTypeParams tpe: List[TypeString[_]] => TypeString[T]
  ) extends TypedMetadata[T] {
    def repr(targs: List[TypeString[_]]): String =
      s"$name: ${tpe(targs)}"
  }

  case class Method[T](
    @reifyName name: String,
    @multi @rpcTypeParamMetadata typeParams: List[TypeParam],
    @multi @rpcParamMetadata params: List[Param[_]],
    @infer @forTypeParams result: List[TypeString[_]] => TypeString[T]
  ) extends TypedMetadata[T] {
    def repr(targs: List[TypeString[_]]): String = {
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
  implicit val meta: GenericMeta[GenericTrait[_, _]] = GenericMeta.materialize
}

class GenericMetadataTest extends FunSuite {
  test("generic metadata") {
    assert(GenericTrait.meta.repr ==
      """GenericTrait[A, B] {
        |  method(a: A, int: Int): List[A]
        |  genericMethod[C](map: Map[A, C]): Map[B, C]
        |}""".stripMargin)
  }
}
