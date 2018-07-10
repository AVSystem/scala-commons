package com.avsystem.commons
package serialization

import java.time.Instant
import java.time.format.DateTimeParseException

import com.avsystem.commons.serialization.GenCodec.ReadFailure

object IsoInstant {
  def format(millis: Long): String =
    Instant.ofEpochMilli(millis).toString

  def parse(string: String): Long =
    try Instant.parse(string).toEpochMilli catch {
      case _: DateTimeParseException => throw new ReadFailure(s"invalid ISO instant: $string")
    }
}
