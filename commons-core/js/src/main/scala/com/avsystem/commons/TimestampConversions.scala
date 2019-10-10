package com.avsystem.commons

import scala.scalajs.js

final class TimestampConversions(private val millis: Long) extends AnyVal {
  def toTimestamp: Timestamp = Timestamp(millis)
  def toJsDate: js.Date = new js.Date(millis.toDouble)
  def toJDate: JDate = new JDate(millis)
}
