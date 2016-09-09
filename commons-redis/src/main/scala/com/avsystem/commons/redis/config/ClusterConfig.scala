package com.avsystem.commons
package redis.config

import akka.util.Timeout
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.actor.RedisConnectionActor.{DebugListener, DevNullListener}
import com.avsystem.commons.redis.{NodeAddress, RedisBatch, RedisOp}

import scala.concurrent.duration._

/**
  * Author: ghik
  * Created: 24/06/16.
  */
case class ClusterConfig(
  nodeConfigs: NodeAddress => NodeConfig = _ => NodeConfig(),
  monitoringConnectionConfigs: NodeAddress => ManagedConnectionConfig = _ => ManagedConnectionConfig(),
  autoRefreshInterval: FiniteDuration = 15.seconds,
  minRefreshInterval: FiniteDuration = 1.seconds,
  nodesToQueryForState: Int => Int = _ min 5,
  maxRedirections: Int = 3
)

case class NodeConfig(
  poolSize: Int = 16,
  initOp: RedisOp[Any] = RedisOp.unit,
  initTimeout: Timeout = Timeout(10.seconds),
  connectionConfigs: Int => ManagedConnectionConfig = _ => ManagedConnectionConfig()
) {
  require(poolSize > 0, "Pool size must be positive")
}

case class ManagedConnectionConfig(
  connectionConfig: ConnectionConfig = ConnectionConfig(),
  reconnectionStrategy: RetryStrategy = ExponentialBackoff(1.seconds, 32.seconds)
)

case class ConnectionConfig(
  initCommands: RedisBatch[Any] = RedisBatch.unit,
  debugListener: DebugListener = DevNullListener
)

trait RetryStrategy {
  def retryDelay(retry: Int): Opt[FiniteDuration]
}

case class ExponentialBackoff(firstDelay: FiniteDuration, maxDelay: FiniteDuration)
  extends RetryStrategy {

  private def expDelay(retry: Int) =
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
  def retryDelay(retry: Int) = Opt.Empty
}
