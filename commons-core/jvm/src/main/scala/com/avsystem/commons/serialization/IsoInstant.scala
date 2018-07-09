package com.avsystem.commons
package serialization

import java.time.Instant
import java.time.format.DateTimeParseException

import com.avsystem.commons.serialization.GenCodec.ReadFailure

object IsoInstant {
  def format(millis: Long): String = {
    val res = Instant.ofEpochMilli(millis).toString
    if (res.length == 20) res.substring(0, 19) + ".000Z" else res
  }

  def parse(string: String): Long =
    try Instant.parse(string).toEpochMilli catch {
      case _: DateTimeParseException => throw new ReadFailure(s"invalid ISO instant: $string")
    }
}
