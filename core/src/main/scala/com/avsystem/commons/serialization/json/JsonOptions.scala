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

/** Selects the algorithm used to read and write JSON numbers (`Double`, `Float`, and the integer types
  * `Byte`/`Short`/`Int`/`Long`).
  */
sealed trait JsonNumberCodec
object JsonNumberCodec {

  /** Reads and writes numbers using the platform (`java.lang.Double.toString`/`parseDouble`,
    * `Integer.parseInt`/`Long.parseLong`, `Int`/`Long` `toString`, ...).
    */
  case object Standard extends JsonNumberCodec

  /** Uses built-in fast number codecs in [[JsonStringInput]] / [[JsonStringOutput]]:
    *
    *   - `Double`/`Float` writes (`XjbDouble`/`XjbFloat`, scalar ports of the xjb algorithm's numeric core) emit the
    *     shortest decimal digits directly into the output, with no intermediate `String`. The text always parses back
    *     to the exact same value, but on JDKs older than 19 it may differ character-wise from `toString` for edge-case
    *     values where the platform was not yet shortest (fixed by Schubfach in JDK 19, see
    *     [[https://bugs.openjdk.org/browse/JDK-4511638 JDK-4511638]]).
    *   - `Double`/`Float` reads (`EiselLemireDouble`/`EiselLemireFloat`, the Eisel-Lemire algorithm) parse straight
    *     from the input buffer with no substring allocation and always return exactly what
    *     `Double.parseDouble`/`Float.parseFloat` would.
    *   - `Byte`/`Short`/`Int`/`Long` reads parse straight from the input buffer, falling back to the platform path for
    *     non-integer literals ("1.0", "1e3") and overflow — results are always identical to [[Standard]].
    *
    * `BigInt` and `BigDecimal` are unaffected.
    */
  case object Fast extends JsonNumberCodec
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
  * @param numberCodec
  *   algorithm used to read and write JSON numbers (see [[JsonNumberCodec]])
  */
case class JsonOptions(
  formatting: JsonFormatting = JsonFormatting.Compact,
  asciiOutput: Boolean = false,
  mathContext: MathContext = BigDecimal.defaultMathContext,
  dateFormat: JsonDateFormat = JsonDateFormat.IsoInstant,
  binaryFormat: JsonBinaryFormat = JsonBinaryFormat.ByteArray,
  numberCodec: JsonNumberCodec = JsonNumberCodec.Standard,
) {

  /** Binary-compatibility constructor (signature from before `numberCodec` was added). */
  def this(
    formatting: JsonFormatting,
    asciiOutput: Boolean,
    mathContext: MathContext,
    dateFormat: JsonDateFormat,
    binaryFormat: JsonBinaryFormat,
  ) = this(formatting, asciiOutput, mathContext, dateFormat, binaryFormat, JsonNumberCodec.Standard)

  // Declaring any `copy` suppresses the synthetic one, so the full-arity variant is spelled out by hand.
  def copy(
    formatting: JsonFormatting = formatting,
    asciiOutput: Boolean = asciiOutput,
    mathContext: MathContext = mathContext,
    dateFormat: JsonDateFormat = dateFormat,
    binaryFormat: JsonBinaryFormat = binaryFormat,
    numberCodec: JsonNumberCodec = numberCodec,
  ): JsonOptions = JsonOptions(formatting, asciiOutput, mathContext, dateFormat, binaryFormat, numberCodec)

  /** Binary-compatibility overload (signature from before `numberCodec` was added). Keeps the current `numberCodec`. */
  def copy(
    formatting: JsonFormatting,
    asciiOutput: Boolean,
    mathContext: MathContext,
    dateFormat: JsonDateFormat,
    binaryFormat: JsonBinaryFormat,
  ): JsonOptions = JsonOptions(formatting, asciiOutput, mathContext, dateFormat, binaryFormat, numberCodec)
}
object JsonOptions {
  final val Default = JsonOptions()
  final val Pretty = JsonOptions(formatting = JsonFormatting.Pretty)

  /** Binary-compatibility overload (signature from before `numberCodec` was added). */
  def apply(
    formatting: JsonFormatting,
    asciiOutput: Boolean,
    mathContext: MathContext,
    dateFormat: JsonDateFormat,
    binaryFormat: JsonBinaryFormat,
  ): JsonOptions = JsonOptions(formatting, asciiOutput, mathContext, dateFormat, binaryFormat, JsonNumberCodec.Standard)
}

case class JsonFormatting(
  indentSize: OptArg[Int] = OptArg.Empty,
  afterColon: Int = 0,
)
object JsonFormatting {
  final val Compact = JsonFormatting()
  final val Pretty = JsonFormatting(indentSize = 2, afterColon = 1)
}
