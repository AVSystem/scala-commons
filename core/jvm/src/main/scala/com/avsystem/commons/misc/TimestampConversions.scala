package com.avsystem.commons.misc

import java.time.Instant

import com.avsystem.commons.JDate

into opaque type TimestampConversions = Long

object TimestampConversions {
  def apply(millis: Long): TimestampConversions = millis

  extension (millis: TimestampConversions) {
    def toTimestamp: Timestamp = Timestamp(millis)
    def toInstant: Instant = Instant.ofEpochMilli(millis)
    def toJDate: JDate = new JDate(millis)
  }

}
