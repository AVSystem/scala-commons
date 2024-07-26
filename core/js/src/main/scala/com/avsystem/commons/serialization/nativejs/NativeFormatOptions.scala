package com.avsystem.commons
package serialization.nativejs

import com.avsystem.commons.misc.{AbstractValueEnum, AbstractValueEnumCompanion, EnumCtx}

/**
  * Specifies format used by `NativeJsonOutput.writeLong` / `NativeJsonInput.readLong`
  * to represent [[Long]]. JS does not support 64-bit representation.
  */
final class NativeLongFormat(implicit ctx: EnumCtx) extends AbstractValueEnum
object NativeLongFormat extends AbstractValueEnumCompanion[NativeLongFormat] {
  final val RawString: Value = new NativeLongFormat
  final val JsNumber: Value = new NativeLongFormat
  final val JsBigInt: Value = new NativeLongFormat
}

/**
  * Specifies format used by `NativeJsonOutput.writeTimestamp` / `NativeJsonInput.readTimestamp`
  * to represent timestamps.
  */
final class NativeDateFormat(implicit ctx: EnumCtx) extends AbstractValueEnum
object NativeDateFormat extends AbstractValueEnumCompanion[NativeDateFormat] {
  final val RawString: Value = new NativeDateFormat
  final val JsNumber: Value = new NativeDateFormat
  final val JsDate: Value = new NativeDateFormat
}

/**
  * Adjusts format produced by [[NativeJsonOutput]].
  *
  * @param longFormat format used to [[Long]]
  * @param dateFormat format used to represent timestamps
  */
final case class NativeFormatOptions(
  longFormat: NativeLongFormat = NativeLongFormat.RawString,
  dateFormat: NativeDateFormat = NativeDateFormat.RawString,
)
object NativeFormatOptions {
  final val RawString = NativeFormatOptions()
}