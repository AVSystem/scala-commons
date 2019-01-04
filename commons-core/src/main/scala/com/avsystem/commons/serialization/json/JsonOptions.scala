package com.avsystem.commons
package serialization.json

import java.math.MathContext

/**
  * Specifies format used by `JsonStringOutput.writeBinary` / `JsonStringInput.readBinary` to represent byte arrays.
  */
sealed trait JsonBinaryFormat
object JsonBinaryFormat {
  /**
    * Specifies that binary data should be represented as JSON array of numeric, signed byte values.
    */
  case object ByteArray extends JsonBinaryFormat

  /**
    * Specifies that binary data should be represented as JSON lowercase hex string. When parsing,
    * uppercase hex digits are also accepted.
    */
  case object HexString extends JsonBinaryFormat

  /**
    * Specifies that binary data should be represented as JSON string containing binary data encoded as
    * Base64. When `withoutPadding` is true, padding characters will be omitted at the end.
    * When `urlSafe` is true, an URL and filename safe alphabet will be used as defined in
    * [[https://tools.ietf.org/html/rfc4648#section-5 RFC4648 Section 5]].
    */
  case class Base64(withoutPadding: Boolean = false, urlSafe: Boolean = false) extends JsonBinaryFormat
}

/**
  * Specifies format used by `JsonStringOutput.writeTimestamp` / `JsonStringInput.readTimestamp`
  * to represent timestamps.
  */
sealed trait JsonDateFormat
object JsonDateFormat {
  /**
    * Specifies that a timestamp should be represented in ISO 8601 format with UTC time zone,
    * e.g. `2012-02-13T07:30:21.232Z`
    */
  case object IsoInstant extends JsonDateFormat
  /**
    * Specifies that a timestamp should be represented as JSON number containing number of milliseconds
    * since UNIX epoch.
    */
  case object EpochMillis extends JsonDateFormat
}

/**
  * Adjusts format of JSON produced by [[JsonStringOutput]].
  *
  * @param formatting   JSON formatting options, controls how whitespace is added to JSON output
  * @param asciiOutput  when set, all non-ASCII characters in strings will be unicode-escaped
  * @param mathContext  `MathContext` used when deserializing `BigDecimal`s
  * @param dateFormat   format used to represent timestamps
  * @param binaryFormat format used to represent binary data (byte arrays)
  */
case class JsonOptions(
  formatting: JsonFormatting = JsonFormatting.Compact,
  asciiOutput: Boolean = false,
  mathContext: MathContext = BigDecimal.defaultMathContext,
  dateFormat: JsonDateFormat = JsonDateFormat.IsoInstant,
  binaryFormat: JsonBinaryFormat = JsonBinaryFormat.ByteArray
)
object JsonOptions {
  final val Default = JsonOptions()
  final val Pretty = JsonOptions(formatting = JsonFormatting.Pretty)
}

case class JsonFormatting(
  indentSize: OptArg[Int] = OptArg.Empty,
  afterColon: Int = 0
)
object JsonFormatting {
  final val Compact = JsonFormatting()
  final val Pretty = JsonFormatting(indentSize = 2, afterColon = 1)
}
