package com.avsystem.commons.misc

import scala.scalajs.js
import com.avsystem.commons.JDate

into opaque type TimestampConversions = Long

object TimestampConversions {
  def apply(millis: Long): TimestampConversions = millis

  extension (millis: TimestampConversions) {
    def toTimestamp: Timestamp = Timestamp(millis)
    def toJsDate: js.Date = new js.Date(millis.toDouble)
    def toJDate: JDate = new JDate(millis)
  }
}
