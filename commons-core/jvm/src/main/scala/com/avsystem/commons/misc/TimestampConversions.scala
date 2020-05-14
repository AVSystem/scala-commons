package com.avsystem.commons.misc

import java.time.Instant

import com.avsystem.commons.JDate

final class TimestampConversions(private val millis: Long) extends AnyVal {
  def toTimestamp: Timestamp = Timestamp(millis)
  def toInstant: Instant = Instant.ofEpochMilli(millis)
  def toJDate: JDate = new JDate(millis)
}
