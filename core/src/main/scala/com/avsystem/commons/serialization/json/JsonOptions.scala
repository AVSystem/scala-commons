package com.avsystem.commons
package serialization.json

import java.math.MathContext

/** Specifies format used by `JsonStringOutput.writeBinary` / `JsonStringInput.readBinary` to represent byte arrays.
  */
sealed trait JsonBinaryFormat
object JsonBinaryFormat {

  /** Specifies that binary data should be represented as JSON array of numeric, signed byte values.
    */
  case object ByteArray extends JsonBinaryFormat

  /** Specifies that binary data should be represented as JSON lowercase hex string. When parsing, uppercase hex digits
    * are also accepted.
    */
  case object HexString extends JsonBinaryFormat

  /** Specifies that binary data should be represented as JSON string containing binary data encoded as Base64. When
    * `withoutPadding` is true, padding characters will be omitted at the end. When `urlSafe` is true, an URL and
    * filename safe alphabet will be used as defined in
    * [[https://tools.ietf.org/html/rfc4648#section-5 RFC4648 Section 5]].
    */
  case class Base64(withoutPadding: Boolean = false, urlSafe: Boolean = false) extends JsonBinaryFormat
}

/** Specifies format used by `JsonStringOutput.writeTimestamp` / `JsonStringInput.readTimestamp` to represent
  * timestamps.
  */
sealed trait JsonDateFormat
object JsonDateFormat {

  /** Specifies that a timestamp should be represented in ISO 8601 format with UTC time zone, e.g.
    * `2012-02-13T07:30:21.232Z`
    */
  case object IsoInstant extends JsonDateFormat

  /** Specifies that a timestamp should be represented as JSON number containing number of milliseconds since UNIX
    * epoch.
    */
  case object EpochMillis extends JsonDateFormat
}

/** Specifies how `JsonStringInput.readByte` / `readShort` / `readInt` / `readLong` read a JSON number that is not a
  * plain integer (e.g. `2.0` or `5e3`). Plain integers (an optional `-` followed by decimal digits) are read the same
  * way in both modes.
  *
  * Because `readTimestamp` with `JsonDateFormat.EpochMillis` reads through `readLong` and `readBinary` with
  * `JsonBinaryFormat.ByteArray` reads through `readByte`, this option applies to them as well. `readBigInt` is not
  * affected.
  */
sealed trait JsonIntegerReading
object JsonIntegerReading {

  /** The number is converted to `Double` (`Byte`, `Short`, `Int`) or to `BigDecimal` with `JsonOptions.mathContext`
    * (`Long`), and accepted when the rounded result is a whole number in range. Because of the rounding, a number that
    * is not whole but close to one, e.g. `2.0000000000000001`, is accepted as that whole number. This is the default.
    */
  case object Rounding extends JsonIntegerReading

  /** A number that is not a plain integer is accepted only when its exact value is a whole number in range, e.g. `2.0`,
    * `5e3` or `-0.0`; otherwise (e.g. `2.0000000000000001`) reading fails. Such a number must follow the JSON number
    * grammar with ASCII digits, with an optional leading `+` and leading zeros allowed, also when it is encoded as a
    * JSON string. The check works on the number's text, in time linear in its length, without `Double` or `BigDecimal`.
    */
  case object Exact extends JsonIntegerReading
}

/** Adjusts format of JSON produced by [[JsonStringOutput]].
  *
  * @param formatting
  *   JSON formatting options, controls how whitespace is added to JSON output
  * @param asciiOutput
  *   when set, all non-ASCII characters in strings will be unicode-escaped
  * @param mathContext
  *   `MathContext` used when deserializing `BigDecimal`s
  * @param dateFormat
  *   format used to represent timestamps
  * @param binaryFormat
  *   format used to represent binary data (byte arrays)
  * @param integerReading
  *   how `Byte`, `Short`, `Int` and `Long` values are read from JSON numbers that are not plain integers (`BigInt` is
  *   not affected), see [[JsonIntegerReading]]
  */
case class JsonOptions(
  formatting: JsonFormatting = JsonFormatting.Compact,
  asciiOutput: Boolean = false,
  mathContext: MathContext = BigDecimal.defaultMathContext,
  dateFormat: JsonDateFormat = JsonDateFormat.IsoInstant,
  binaryFormat: JsonBinaryFormat = JsonBinaryFormat.ByteArray,
  integerReading: JsonIntegerReading = JsonIntegerReading.Rounding,
) {
  // The 5-parameter constructor and `copy` below keep binary compatibility with versions before `integerReading` was
  // added
  def this(
    formatting: JsonFormatting,
    asciiOutput: Boolean,
    mathContext: MathContext,
    dateFormat: JsonDateFormat,
    binaryFormat: JsonBinaryFormat,
  ) = this(formatting, asciiOutput, mathContext, dateFormat, binaryFormat, JsonIntegerReading.Rounding)

  def copy(
    formatting: JsonFormatting,
    asciiOutput: Boolean,
    mathContext: MathContext,
    dateFormat: JsonDateFormat,
    binaryFormat: JsonBinaryFormat,
  ): JsonOptions = new JsonOptions(formatting, asciiOutput, mathContext, dateFormat, binaryFormat, integerReading)

  // a user-defined `copy` suppresses the synthetic one, so the 6-parameter `copy` is written out too
  def copy(
    formatting: JsonFormatting = formatting,
    asciiOutput: Boolean = asciiOutput,
    mathContext: MathContext = mathContext,
    dateFormat: JsonDateFormat = dateFormat,
    binaryFormat: JsonBinaryFormat = binaryFormat,
    integerReading: JsonIntegerReading = integerReading,
  ): JsonOptions = new JsonOptions(formatting, asciiOutput, mathContext, dateFormat, binaryFormat, integerReading)
}
object JsonOptions {
  // keeps binary compatibility with versions before `integerReading` was added
  def apply(
    formatting: JsonFormatting,
    asciiOutput: Boolean,
    mathContext: MathContext,
    dateFormat: JsonDateFormat,
    binaryFormat: JsonBinaryFormat,
  ): JsonOptions = new JsonOptions(formatting, asciiOutput, mathContext, dateFormat, binaryFormat)

  final val Default = JsonOptions()
  final val Pretty = JsonOptions(formatting = JsonFormatting.Pretty)
}

case class JsonFormatting(
  indentSize: OptArg[Int] = OptArg.Empty,
  afterColon: Int = 0,
)
object JsonFormatting {
  final val Compact = JsonFormatting()
  final val Pretty = JsonFormatting(indentSize = 2, afterColon = 1)
}
