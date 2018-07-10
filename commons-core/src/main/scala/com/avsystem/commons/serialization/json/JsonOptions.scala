package com.avsystem.commons
package serialization.json

import java.math.MathContext

/**
  * Specifies format used by `JsonStringOutput.writeBinary` / `JsonStringInput.readBinary` to represent byte arrays.
  */
sealed trait JsonBinaryFormat
object JsonBinaryFormat {
  /** Specifies that binary data should be represented as JSON array of numeric, signed byte values. */
  case object ByteArray extends JsonBinaryFormat
  /** Specifies that binary data should be represented as JSON lowercase hex string */
  case object HexString extends JsonBinaryFormat
}

/**
  * Specifies format used by `JsonStringOutput.writeTimestamp` / `JsonStringInput.readTimestamp`
  * to represent timestamps.
  */
sealed trait JsonDateFormat
object JsonDateFormat {
  /**
    * Specifies that a timestamp should be represented in ISO 8861 format with UTC time zone,
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
  * @param indentSize   Number of spaces to indent every object field and array element with. When undefined, resulting
  *                     JSON is compact - it contains no whitespaces.
  * @param asciiOutput  when set, all non-ASCII characters in strings will be unicode-escaped
  * @param mathContext  `MathContext` used when deserializing `BigDecimal`s
  * @param dateFormat   format used to represent timestamps
  * @param binaryFormat format used to represent binary data (byte arrays)
  */
case class JsonOptions(
  indentSize: OptArg[Int] = OptArg.Empty,
  asciiOutput: Boolean = false,
  mathContext: MathContext = BigDecimal.defaultMathContext,
  dateFormat: JsonDateFormat = JsonDateFormat.IsoInstant,
  binaryFormat: JsonBinaryFormat = JsonBinaryFormat.ByteArray
)
object JsonOptions {
  final val Default = JsonOptions()
}
