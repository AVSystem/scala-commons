package com.avsystem.commons
package rpc

import upickle.Js
import upickle.default._

case class RawInvocation(rpcName: String, argLists: List[List[Js.Value]])

object RawInvocation {
  implicit val RawInvocationWriter = Writer[RawInvocation] {
    case inv => Js.Obj(
      ("rpcName", Js.Str(inv.rpcName)),
      ("argLists", argsToJsArr(inv.argLists))
    )
  }

  implicit val RawInvocationReader = Reader[RawInvocation] {
    case obj: Js.Obj =>
      val name: String = readJs[String](obj("rpcName"))
      val args: List[List[Js.Value]] = jsArrToArgs(obj("argLists"))
      RawInvocation(name, args)
  }

  def argsToJsArr(argLists: List[List[Js.Value]]): Js.Value = {
    Js.Arr(argLists map { args => Js.Arr(args: _*) }: _*)
  }

  def jsArrToArgs(value: Js.Value): List[List[Js.Value]] = {
    value match {
      case array: Js.Arr =>
        (array.value map {
          case nestedArray: Js.Arr => nestedArray.value.toList
          case _ => List()
        }).toList
      case _ => List()
    }
  }
}
