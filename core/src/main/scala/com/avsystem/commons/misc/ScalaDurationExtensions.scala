package com.avsystem.commons
package misc

import scala.concurrent.duration.{DoubleMult, DurationDouble, DurationInt, DurationLong, IntMult, LongMult}

/**
 * Gathers all extensions from [[scala.concurrent.duration]] into one trait that can be mixed in with package object.
 */
trait ScalaDurationExtensions {
  given Conversion[Int, DurationInt] = new DurationInt(_)
  given Conversion[Long, DurationLong] = new DurationLong(_)
  given Conversion[Double, DurationDouble] = new DurationDouble(_)
  given Conversion[Int, IntMult] = new IntMult(_)
  given Conversion[Long, LongMult] = new LongMult(_)
  given Conversion[Double, DoubleMult] = new DoubleMult(_)
}
object ScalaDurationExtensions extends ScalaDurationExtensions
