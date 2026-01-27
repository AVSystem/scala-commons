package com.avsystem.commons
package concurrent

import scala.concurrent.duration.*

trait DurationPostfixConverters {
  given (int: Int) => DurationInt = new DurationInt(int)
  given (long: Long) => DurationLong = new DurationLong(long)
  given (double: Double) => DurationDouble = new DurationDouble(double)
}
