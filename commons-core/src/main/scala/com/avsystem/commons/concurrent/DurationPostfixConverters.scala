package com.avsystem.commons
package concurrent

import scala.concurrent.duration._

trait DurationPostfixConverters {
  implicit def durationInt(int: Int): DurationInt = new DurationInt(int)
  implicit def durationLong(long: Long): DurationLong = new DurationLong(long)
  implicit def durationDouble(double: Double): DurationDouble = new DurationDouble(double)
}
