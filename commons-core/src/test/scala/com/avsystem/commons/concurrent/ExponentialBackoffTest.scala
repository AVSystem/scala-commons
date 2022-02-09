package com.avsystem.commons
package concurrent

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

/**
  * Author: ghik
  * Created: 31/08/16.
  */
class ExponentialBackoffTest extends AnyFunSuite with Matchers {
  test("simple") {
    import RetryStrategy._
    val eb = immediately.andThen(exponentially(1.second)).maxDelay(20.seconds).maxRetries(64)

    val allDelays = Iterator.iterateUntilEmpty(eb.nextRetry)(_._2.nextRetry).map(_._1).toList
    allDelays shouldBe List(
      Duration.Zero, 1.second, 2.seconds, 4.seconds, 8.seconds, 16.seconds, 20.seconds, 20.seconds,
      20.seconds, 20.seconds, 20.seconds, 20.seconds, 20.seconds, 20.seconds, 20.seconds, 20.seconds,
      20.seconds, 20.seconds, 20.seconds, 20.seconds, 20.seconds, 20.seconds, 20.seconds, 20.seconds,
      20.seconds, 20.seconds, 20.seconds, 20.seconds, 20.seconds, 20.seconds, 20.seconds, 20.seconds,
      20.seconds, 20.seconds, 20.seconds, 20.seconds, 20.seconds, 20.seconds, 20.seconds, 20.seconds,
      20.seconds, 20.seconds, 20.seconds, 20.seconds, 20.seconds, 20.seconds, 20.seconds, 20.seconds,
      20.seconds, 20.seconds, 20.seconds, 20.seconds, 20.seconds, 20.seconds, 20.seconds, 20.seconds,
      20.seconds, 20.seconds, 20.seconds, 20.seconds, 20.seconds, 20.seconds, 20.seconds, 20.seconds,
    )
  }
}
