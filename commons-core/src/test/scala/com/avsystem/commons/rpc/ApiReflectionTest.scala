package com.avsystem.commons
package rpc

import com.avsystem.commons.meta._
import com.avsystem.commons.misc.TypeString
import org.scalatest.funsuite.AnyFunSuite

class cool extends StaticAnnotation

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
  @reifyFlags flags: MethodFlags,
  @isAnnotated[cool] cool: Boolean,
  @reifyParamListCount paramListCount: Int,
  @multi @rpcTypeParamMetadata typeParams: List[TypeParamInfo],
  @multi @rpcParamMetadata params: List[ParamInfo[_]],
  @forTypeParams @infer resultTs: List[TypeString[_]] => TypeString[T]
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

  def repr: String = {
    val typeParamsRepr = typeParams.map(_.name).mkStringOrEmpty("[", ", ", "]")
    val paramsRepr = paramLists.map(_.map(_.repr(typeParams)).mkString("(", ", ", ")")).mkString
    val resultTypeString = resultTs(typeParams.map(_.typeString))
    val coolRepr = if(cool) "@cool " else ""
    s"$coolRepr${flags.baseDecl} $name$typeParamsRepr$paramsRepr: $resultTypeString"
  }
}

case class TypeParamInfo(
  @reifyName name: String
) {
  def typeString: TypeString[_] = new TypeString(name)
}

case class ParamInfo[T](
  @reifyName name: String,
  @reifyPosition pos: ParamPosition,
  @reifyFlags flags: ParamFlags,
  @forTypeParams @infer ts: List[TypeString[_]] => TypeString[T]
) extends TypedMetadata[T] {
  def repr(tparams: List[TypeParamInfo]): String = {
    val implicitMod = if (pos.indexInList == 0 && flags.isImplicit) "implicit " else ""
    s"$implicitMod$name: ${ts(tparams.map(_.typeString))}"
  }
}

class SimpleApi {
  final val Thing = "fuu"
  @cool lazy val CoolThing = -1
  def noParamLists: Int = 42
  def noParams(): String = ""
  def multiParamLists(int: Int)(str: String)(): Double = int.toDouble
  def takesImplicits(int: Int)(implicit ord: Ordering[Int], moar: DummyImplicit): String = int.toString
  def takesTypeArgs[A, B](as: List[A], bs: Set[B]): Map[A, B] = Map.empty
  @ignore def completelyIgnoredMethod(int: Int, string: String): Unit = ()
}

class ApiReflectionTest extends AnyFunSuite {
  test("Date API") {
    assert(ApiInfo.materialize[JDate].repr ==
      """java.util.Date {
        |  def getYear(): Int
        |  def setYear(x$1: Int): Unit
        |  def getMonth(): Int
        |  def setMonth(x$1: Int): Unit
        |  def getDate(): Int
        |  def setDate(x$1: Int): Unit
        |  def getDay(): Int
        |  def getHours(): Int
        |  def setHours(x$1: Int): Unit
        |  def getMinutes(): Int
        |  def setMinutes(x$1: Int): Unit
        |  def getSeconds(): Int
        |  def setSeconds(x$1: Int): Unit
        |  def getTime(): Long
        |  def setTime(x$1: Long): Unit
        |  def before(x$1: java.util.Date): Boolean
        |  def after(x$1: java.util.Date): Boolean
        |  def compareTo(x$1: java.util.Date): Int
        |  def toLocaleString(): String
        |  def toGMTString(): String
        |  def getTimezoneOffset(): Int
        |  def toInstant(): java.time.Instant
        |}""".stripMargin
    )
  }

  test("Simple API") {
    assert(ApiInfo.materialize[SimpleApi].repr ==
      """com.avsystem.commons.rpc.SimpleApi {
        |  final val Thing: String
        |  @cool lazy val CoolThing: Int
        |  def noParamLists: Int
        |  def noParams(): String
        |  def multiParamLists(int: Int)(str: String)(): Double
        |  def takesImplicits(int: Int)(implicit ord: Ordering[Int], moar: DummyImplicit): String
        |  def takesTypeArgs[A, B](as: List[A], bs: Set[B]): Map[A, B]
        |}""".stripMargin)
  }
}
