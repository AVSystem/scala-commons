package com.avsystem.commons
package redis.protocol

import scala.annotation.nowarn
import org.scalacheck.Shrink

/** Author: ghik Created: 04/04/16.
  */
object CommonsScalacheck {
  @nowarn("msg=deprecated")
  implicit def shrinkOpt[T: Shrink]: Shrink[Opt[T]] = Shrink {
    case Opt.Empty => Stream.empty
    case Opt(x) => Stream.cons(Opt.Empty, for (y <- Shrink.shrink(x)) yield Opt(y))
  }
}
