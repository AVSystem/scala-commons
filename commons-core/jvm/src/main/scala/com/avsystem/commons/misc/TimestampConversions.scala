package com.avsystem.commons
package misc

import java.time.Instant

final class TimestampConversions(private val millis: Long) extends AnyVal {
  def toInstant: Instant = Instant.ofEpochMilli(millis)
  def toJDate: JDate = new JDate(millis)
}
