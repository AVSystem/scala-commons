package com.avsystem.commons
package redis.protocol

import com.avsystem.commons.misc.Opt
import org.scalacheck.Shrink

/**
  * Author: ghik
  * Created: 04/04/16.
  */
object CommonsScalacheck {
  implicit def shrinkOpt[T: Shrink]: Shrink[Opt[T]] = Shrink {
    case Opt.Empty => Stream.empty
    case Opt(x) => Stream.cons(Opt.Empty, for (y <- Shrink.shrink(x)) yield Opt(y))
  }
}
