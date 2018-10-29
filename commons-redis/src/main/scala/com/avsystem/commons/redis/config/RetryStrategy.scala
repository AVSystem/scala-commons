package com.avsystem.commons
package redis.config

import scala.concurrent.duration.{Duration, FiniteDuration}

trait RetryStrategy { self =>
  /**
    * Determines a delay that will be waited before retrying some operation that failed (e.g. Redis connection attempt)
    * and also returns next retry strategy that should be used if that retry itself also fails.
    * If this method returns `Opt.Empty`, the operation will not be retried and failure should be reported.
    */
  def nextRetry: Opt[(FiniteDuration, RetryStrategy)]

  def andThen(otherStrategy: RetryStrategy): RetryStrategy =
    RetryStrategy(self.nextRetry match {
      case Opt((delay, nextStrat)) => Opt((delay, nextStrat andThen otherStrategy))
      case Opt.Empty => otherStrategy.nextRetry
    })

  def maxDelay(duration: FiniteDuration): RetryStrategy =
    RetryStrategy(self.nextRetry.map { case (delay, retry) => (delay min duration, retry.maxDelay(duration)) })

  def maxTotal(duration: FiniteDuration): RetryStrategy =
    RetryStrategy(self.nextRetry.collect {
      case (delay, nextStrat) =>
        if (delay <= duration) (delay, nextStrat.maxTotal(duration - delay))
        else (duration, RetryStrategy.never)
    })

  def maxRetries(retries: Int): RetryStrategy =
    if (retries <= 0) RetryStrategy.never
    else RetryStrategy(self.nextRetry.map { case (delay, nextStrat) => (delay, nextStrat.maxRetries(retries - 1)) })

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
    val nextStrat = firstDelay * factor match {
      case fd: FiniteDuration => exponentially(fd, factor)
      case _ => never
    }
    Opt((firstDelay, nextStrat))
  }
}

object ConfigDefaults {
  val Dispatcher = "redis.pinned-dispatcher"
}
