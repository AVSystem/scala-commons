package com.avsystem.commons
package redis.config

import scala.concurrent.duration.{Duration, FiniteDuration}

trait RetryStrategy {
  /**
    * Determines a delay that will be waited before restarting a failed Redis connection.
    * If this method returns `Opt.Empty`, the connection will not be restarted and will remain in a broken state.
    *
    * @param retry indicates which consecutive reconnection retry it is after the connection was lost, starting from 0
    */
  def retryDelay(retry: Int): Opt[FiniteDuration]
}

case class ExponentialBackoff(firstDelay: FiniteDuration, maxDelay: FiniteDuration)
  extends RetryStrategy {

  private def expDelay(retry: Int): FiniteDuration =
    firstDelay * (1 << (retry - 1))

  private val maxRetry =
    Iterator.from(1).find(i => expDelay(i) >= maxDelay).getOrElse(Int.MaxValue)

  def retryDelay(retry: Int) = Opt {
    if (retry == 0) Duration.Zero
    else if (retry >= maxRetry) maxDelay
    else expDelay(retry)
  }
}

case object NoRetryStrategy extends RetryStrategy {
  def retryDelay(retry: Int): Opt[FiniteDuration] = Opt.Empty
}

object ConfigDefaults {
  val Dispatcher = "redis.pinned-dispatcher"
}
