package com.avsystem.commons
package serialization.nativejs

import com.avsystem.commons.misc.{AbstractValueEnum, AbstractValueEnumCompanion, EnumCtx}

/**
 * Specifies format used by `NativeJsonOutput.writeLong` / `NativeJsonInput.readLong` to represent [[Long]]. JS does
 * not support 64-bit representation.
 */
enum NativeLongFormat {
  case RawString, JsNumber, JsBigInt
}

/**
 * Specifies format used by `NativeJsonOutput.writeTimestamp` / `NativeJsonInput.readTimestamp` to represent
 * timestamps.
 */
enum NativeDateFormat {
  case RawString, JsNumber, JsDate
}

/**
 * Specifies format used by `NativeJsonOutput.writeBigInt` / `NativeJsonInput.readBigInt` to represent [[BigInt]].
 *
 * Note that [[scala.scalajs.js.JSON.stringify]] does not know how to serialize a BigInt and throws an error
 */
enum NativeBigIntFormat {
  case RawString, JsBigInt
}

/**
 * Adjusts format produced by [[NativeJsonOutput]].
 *
 * @param longFormat
 *   format used to [[Long]]
 * @param dateFormat
 *   format used to represent timestamps
 * @param bigIntFormat
 *   format used to represent [[BigInt]]
 */
final case class NativeFormatOptions(
  longFormat: NativeLongFormat = NativeLongFormat.RawString,
  dateFormat: NativeDateFormat = NativeDateFormat.RawString,
  bigIntFormat: NativeBigIntFormat = NativeBigIntFormat.RawString,
)
object NativeFormatOptions {
  final val RawString = NativeFormatOptions()
}
