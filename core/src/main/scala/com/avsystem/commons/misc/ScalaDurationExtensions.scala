package com.avsystem.commons
package misc

import scala.concurrent.duration.{DoubleMult, DurationDouble, DurationInt, DurationLong, IntMult, LongMult}

/**
  * Gathers all extensions from [[scala.concurrent.duration]] into one trait that can be mixed in with package object.
  */
trait ScalaDurationExtensions {
  implicit def durationIntOps(n: Int): DurationInt = new DurationInt(n)
  implicit def durationLongOps(n: Long): DurationLong = new DurationLong(n)
  implicit def durationDoubleOps(d: Double): DurationDouble = new DurationDouble(d)
  implicit def durationIntMulOps(i: Int): IntMult = new IntMult(i)
  implicit def durationLongMulOps(i: Long): LongMult = new LongMult(i)
  implicit def durationDoubleMulOps(d: Double): DoubleMult = new DoubleMult(d)
}
object ScalaDurationExtensions extends ScalaDurationExtensions
