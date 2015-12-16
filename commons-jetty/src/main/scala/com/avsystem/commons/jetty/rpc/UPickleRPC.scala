package com.avsystem.commons
package jetty.rpc

import com.avsystem.commons.rpc.RPCFramework
import upickle._

object UPickleRPC extends RPCFramework {
  type RawValue = Js.Value
  type Reader[T] = default.Reader[T]
  type Writer[T] = default.Writer[T]

  def read[T: Reader](raw: RawValue): T = default.readJs[T](raw)
  def write[T: Writer](value: T): RawValue = default.writeJs[T](value)

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
