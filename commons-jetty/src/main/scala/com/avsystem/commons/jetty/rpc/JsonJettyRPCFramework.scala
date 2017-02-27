package com.avsystem.commons
package jetty.rpc

import com.avsystem.commons.serialization.GenCodec
import upickle.Js

object JsonJettyRPCFramework extends JettyRPCFramework {
  type RawValue = Js.Value
  type Reader[T] = GenCodec[T]
  type Writer[T] = GenCodec[T]

  def valueToJson(value: RawValue) = upickle.json.write(value)
  def jsonToValue(json: String) = upickle.json.read(json)
  def argsToJson(args: List[List[RawValue]]) = upickle.json.write(argsToJsArr(args))
  def jsonToArgs(json: String) = jsArrToArgs(upickle.json.read(json))

  def read[T: Reader](raw: RawValue): T = GenCodec.read[T](new JsValueInput(raw))
  def write[T: Writer](value: T): RawValue = JsValueOutput.write[T](value)

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
