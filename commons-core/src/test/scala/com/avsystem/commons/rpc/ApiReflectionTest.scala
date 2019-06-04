package com.avsystem.commons
package rpc

import com.avsystem.commons.meta.{ParamFlags, ParamPosition, TypedMetadata, infer, multi, reifyFlags, reifyName, reifyParamListCount, reifyPosition}
import com.avsystem.commons.misc.TypeString
import com.google.common.io.ByteStreams
import org.scalatest.FunSuite

case class ApiInfo[T](
  @infer ts: TypeString[T],
  @multi @mangleOverloads @rpcMethodMetadata methods: List[MethodInfo[_]]
) extends TypedMetadata[T] {
  def repr = s"${ts.value} {${methods.map(m => "\n  " + m.repr).mkString}\n}"
}
object ApiInfo extends ApiMetadataCompanion[ApiInfo]

case class MethodInfo[T](
  @reifyName name: String,
  @reifyName(useRawName = true) rawName: String,
  @reifyParamListCount paramListCount: Int,
  @multi @rpcParamMetadata params: List[ParamInfo[_]],
  @infer resultTs: TypeString[T]
) extends TypedMetadata[T] {

  val paramLists: List[List[ParamInfo[_]]] = {
    def extract(listIdx: Int, params: List[ParamInfo[_]]): List[List[ParamInfo[_]]] =
      if (listIdx == paramListCount) Nil
      else params.span(_.pos.indexOfList == listIdx) match {
        case (nextList, rest) =>
          nextList :: extract(listIdx + 1, rest)
      }
    extract(0, params)
  }

  def repr = s"def $name${paramLists.map(_.map(_.repr).mkString("(", ", ", ")")).mkString}: ${resultTs.value}"
}

case class ParamInfo[T](
  @reifyName name: String,
  @reifyPosition pos: ParamPosition,
  @reifyFlags flags: ParamFlags,
  @infer ts: TypeString[T]
) extends TypedMetadata[T] {
  def repr: String = {
    val implicitMod = if (pos.indexInList == 0 && flags.isImplicit) "implicit " else ""
    s"$implicitMod$name: ${ts.value}"
  }
}

class SimpleApi {
  def noParamLists: Int = 42
  def noParams(): String = ""
  def multiParamLists(int: Int)(str: String)(): Double = int.toDouble
  def takesImplicits(int: Int)(implicit ord: Ordering[Int], moar: DummyImplicit): String = int.toString
}

class ApiReflectionTest extends FunSuite {
  test("String API") {
    val expected = new String(ByteStreams.toByteArray(getClass.getResourceAsStream("/StringApi.txt")))
    assert(ApiInfo.materialize[String].repr == expected)
  }

  test("Simple API") {
    val expected = new String(ByteStreams.toByteArray(getClass.getResourceAsStream("/SimpleApi.txt")))
    assert(ApiInfo.materialize[SimpleApi].repr == expected)
  }
}
