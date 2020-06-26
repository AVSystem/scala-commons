package com.avsystem.commons
package hocon

import java.time.{Duration, Period}
import java.time.temporal.TemporalAmount

import com.avsystem.commons.serialization.{InputMetadata, TypeMarker}
import com.typesafe.config.{ConfigMemorySize, ConfigValue, ConfigValueType}

sealed trait HoconTypeMarker[T] extends TypeMarker[T]

/**
  * [[com.avsystem.commons.serialization.TypeMarker TypeMarker]] which allows you to read raw
  * `com.typesafe.config.ConfigValue` from a [[HoconInput]] in a
  * [[com.avsystem.commons.serialization.GenCodec GenCodec]] implementation.
  */
object ConfigValueMarker extends HoconTypeMarker[ConfigValue]

/**
  * [[com.avsystem.commons.serialization.TypeMarker TypeMarker]] which allows direct reading of a `Duration`
  * from [[HoconInput]] in a [[com.avsystem.commons.serialization.GenCodec GenCodec]] implementation, taking advantage
  * of the [[https://github.com/lightbend/config/blob/master/HOCON.md#duration-format recommended HOCON format for durations]],
  * implemented by `Config.getDuration`.
  */
object DurationMarker extends HoconTypeMarker[Duration]

/**
  * [[com.avsystem.commons.serialization.TypeMarker TypeMarker]] which allows direct reading of a size in bytes
  * from [[HoconInput]] in a [[com.avsystem.commons.serialization.GenCodec GenCodec]] implementation, taking advantage
  * of the [[https://github.com/lightbend/config/blob/master/HOCON.md#size-in-bytes-format HOCON format for size in bytes]],
  * implemented by `Config.getBytes`.
  */
object SizeInBytesMarker extends HoconTypeMarker[Long]

/**
  * [[com.avsystem.commons.serialization.TypeMarker TypeMarker]] which allows direct reading of a `ConfigMemorySize`
  * from [[HoconInput]] in a [[com.avsystem.commons.serialization.GenCodec GenCodec]] implementation, taking advantage
  * of the [[https://github.com/lightbend/config/blob/master/HOCON.md#size-in-bytes-format HOCON format for size in bytes]],
  * implemented by `Config.getMemorySize`.
  */
object ConfigMemorySizeMarker extends HoconTypeMarker[ConfigMemorySize]

/**
  * [[com.avsystem.commons.serialization.TypeMarker TypeMarker]] which allows direct reading of a `Period`
  * from [[HoconInput]] in a [[com.avsystem.commons.serialization.GenCodec GenCodec]] implementation, taking advantage
  * of the [[https://github.com/lightbend/config/blob/master/HOCON.md#period-format HOCON format for period]],
  * implemented by `Config.getPeriod`.
  */
object PeriodMarker extends HoconTypeMarker[Period]

/**
  * [[com.avsystem.commons.serialization.TypeMarker TypeMarker]] which allows direct reading of a `TemporalAmount`
  * from [[HoconInput]] in a [[com.avsystem.commons.serialization.GenCodec GenCodec]] implementation, taking advantage
  * of the [[https://github.com/lightbend/config/blob/master/HOCON.md#period-format HOCON format for period]],
  * implemented by `Config.getTemporal`.
  */
object TemporalAmountMarker extends HoconTypeMarker[TemporalAmount]

/**
  * [[com.avsystem.commons.serialization.TypeMarker TypeMarker]] which allows direct reading of a `java.lang.Number`
  * from [[HoconInput]] in a [[com.avsystem.commons.serialization.GenCodec GenCodec]] implementation, taking advantage
  * of the parsing implemented by `Config.getNumber`.
  */
object NumberMarker extends HoconTypeMarker[Number]
