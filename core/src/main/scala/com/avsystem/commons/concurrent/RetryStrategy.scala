package com.avsystem.commons
package concurrent

import scala.concurrent.duration._

/**
  * A `RetryStrategy` is conceptually a lazy sequence of delays, possibly infinite.
  */
trait RetryStrategy { self =>
  /**
    * Determines a delay that will be waited before retrying some operation that failed (e.g. Redis connection attempt)
    * and also returns next retry strategy that should be used if that retry itself also fails.
    * If this method returns `Opt.Empty`, the operation will not be retried and failure should be reported.
    */
  def nextRetry: Opt[(FiniteDuration, RetryStrategy)]

  /**
    * Concatenates two retry strategies, understood as lazy sequences of delays.
    */
  def andThen(otherStrategy: RetryStrategy): RetryStrategy =
    RetryStrategy(self.nextRetry match {
      case Opt((delay, nextStrat)) => Opt((delay, nextStrat andThen otherStrategy))
      case Opt.Empty => otherStrategy.nextRetry
    })

  /**
    * Limits the maximum delay between retries to some specified duration.
    */
  def maxDelay(duration: FiniteDuration): RetryStrategy =
    RetryStrategy(self.nextRetry.map { case (delay, retry) => (delay min duration, retry.maxDelay(duration)) })

  /**
    * Limits the maximum total duration (sum of retry delays) to some specified duration.
    */
  def maxTotal(duration: FiniteDuration): RetryStrategy =
    RetryStrategy(self.nextRetry.collect {
      case (delay, nextStrat) =>
        if (delay <= duration) (delay, nextStrat.maxTotal(duration - delay))
        else (duration, RetryStrategy.never)
    })

  /**
    * Limits the maximum number of retries to some specified number.
    */
  def maxRetries(retries: Int): RetryStrategy =
    if (retries <= 0) RetryStrategy.never
    else RetryStrategy(self.nextRetry.map { case (delay, nextStrat) => (delay, nextStrat.maxRetries(retries - 1)) })

  /**
    * Randomizes delays. Each delay is multiplied by a factor which is randomly and uniformly choosen from
    * specified segment `[minFactor, maxFactor)` (e.g. 0.9 to 1.1)
    */
  def randomized(minFactor: Double, maxFactor: Double): RetryStrategy =
    RetryStrategy(self.nextRetry.flatMap { case (delay, nextStrat) =>
      val factor = minFactor + (maxFactor - minFactor) * math.random()
      delay * factor match {
        case fd: FiniteDuration => Opt((fd, nextStrat.randomized(minFactor, maxFactor)))
        case _ => Opt.Empty
      }
    })

  def next: RetryStrategy =
    nextRetry.fold(RetryStrategy.never) { case (_, n) => n }
}
object RetryStrategy {
  def apply(nextRetryThunk: => Opt[(FiniteDuration, RetryStrategy)]): RetryStrategy =
    new RetryStrategy {
      def nextRetry: Opt[(FiniteDuration, RetryStrategy)] = nextRetryThunk
    }

  def never: RetryStrategy =
    apply(Opt.Empty)

  def immediately: RetryStrategy =
    once(Duration.Zero)

  def times(count: Int, duration: FiniteDuration = Duration.Zero): RetryStrategy =
    if (count <= 0) never else apply(Opt(duration, times(count - 1, duration)))

  def once(delay: FiniteDuration): RetryStrategy =
    apply(Opt((delay, never)))

  def continually(delay: FiniteDuration): RetryStrategy =
    apply(Opt((delay, continually(delay))))

  def exponentially(firstDelay: FiniteDuration, factor: Double = 2): RetryStrategy = apply {
    val nextNanos = firstDelay.toNanos * factor
    val nextStrat =
      if (nextNanos > Long.MaxValue) continually(Long.MaxValue.nanos)
      else exponentially(nextNanos.nanos, factor)
    Opt((firstDelay, nextStrat))
  }
}
