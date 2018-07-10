package com.avsystem.commons
package serialization

import java.time.Instant
import java.time.format.DateTimeParseException

import com.avsystem.commons.serialization.GenCodec.ReadFailure

object IsoInstant {
  def format(millis: Long): String = {
    val res = Instant.ofEpochMilli(millis).toString
    // add trailing .000Z if omitted to align with JS implementation
    if (res.charAt(res.length - 5) == '.') res
    else res.substring(0, res.length - 1) + ".000Z"
  }

  def parse(string: String): Long =
    try Instant.parse(string).toEpochMilli catch {
      case _: DateTimeParseException => throw new ReadFailure(s"invalid ISO instant: $string")
    }
}
