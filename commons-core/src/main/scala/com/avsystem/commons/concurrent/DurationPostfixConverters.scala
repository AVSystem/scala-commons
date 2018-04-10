package com.avsystem.commons
package concurrent

import scala.concurrent.duration._

trait DurationPostfixConverters {
  implicit def durationInt(int: Int): DurationInt = int
  implicit def durationLong(long: Long): DurationLong = long
  implicit def durationLong(double: Double): DurationDouble = double
}
