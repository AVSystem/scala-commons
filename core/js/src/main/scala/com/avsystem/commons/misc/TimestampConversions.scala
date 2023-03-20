package com.avsystem.commons.misc

import scala.scalajs.js
import com.avsystem.commons.JDate

final class TimestampConversions(private val millis: Long) extends AnyVal {
  def toTimestamp: Timestamp = Timestamp(millis)
  def toJsDate: js.Date = new js.Date(millis.toDouble)
  def toJDate: JDate = new JDate(millis)
}
