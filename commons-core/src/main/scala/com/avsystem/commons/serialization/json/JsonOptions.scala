package com.avsystem.commons
package serialization.json

sealed trait JsonBinaryFormat
object JsonBinaryFormat {
  case object ByteArray extends JsonBinaryFormat
  case object HexString extends JsonBinaryFormat
}

sealed trait JsonDateFormat
object JsonDateFormat {
  case object IsoInstant extends JsonDateFormat
  case object EpochMillis extends JsonDateFormat
}

case class JsonOptions(
  asciiOutput: Boolean = false,
  dateFormat: JsonDateFormat = JsonDateFormat.IsoInstant,
  binaryFormat: JsonBinaryFormat = JsonBinaryFormat.ByteArray
)
object JsonOptions {
  final val Default = JsonOptions()
}
