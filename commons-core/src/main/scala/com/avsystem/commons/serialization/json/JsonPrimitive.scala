package com.avsystem.commons
package serialization.json

import com.avsystem.commons.serialization.TypeMarker

sealed trait JsonPrimitive
object JsonPrimitive {
  case object Null extends JsonPrimitive
  case class Bool(value: Boolean) extends JsonPrimitive
  case class Str(value: String) extends JsonPrimitive
  case class Num(value: String) extends JsonPrimitive

  object Marker extends TypeMarker[JsonPrimitive]
}
