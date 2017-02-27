package com.avsystem.commons
package redis.config

import org.scalatest.{FunSuite, Matchers}

import scala.concurrent.duration._

/**
  * Author: ghik
  * Created: 31/08/16.
  */
class ExponentialBackoffTest extends FunSuite with Matchers {
  test("simple") {
    val eb = ExponentialBackoff(1.second, 20.seconds)

    (0 to 7).map(eb.retryDelay) shouldBe
      Seq(Duration.Zero, 1.second, 2.seconds, 4.seconds, 8.seconds, 16.seconds, 20.seconds, 20.seconds).map(Opt(_))
  }
}
