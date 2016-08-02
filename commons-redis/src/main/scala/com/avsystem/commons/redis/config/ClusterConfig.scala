package com.avsystem.commons
package redis.config

import akka.util.Timeout
import com.avsystem.commons.redis.actor.RedisConnectionActor.{DebugListener, DevNullListener}
import com.avsystem.commons.redis.{NodeAddress, RedisBatch, RedisOp}

import scala.concurrent.duration._

/**
  * Author: ghik
  * Created: 24/06/16.
  */
case class ClusterConfig(
  autoRefreshInterval: FiniteDuration = 30.seconds,
  minRefreshInterval: FiniteDuration = 5.seconds,
  nodesToQueryForState: Int => Int = _ min 5,
  nodeClientCloseDelay: FiniteDuration = 5.seconds,
  maxRedirections: Int = 3,
  monitoringConnectionConfigs: NodeAddress => ConnectionConfig = _ => ConnectionConfig(),
  nodeConfigs: NodeAddress => NodeConfig = _ => NodeConfig()
)

case class NodeConfig(
  poolSize: Int = 16,
  initOp: RedisOp[Any] = RedisOp.unit,
  initTimeout: Timeout = Timeout(10.seconds),
  connectionConfigs: Int => ConnectionConfig = _ => ConnectionConfig()
) {
  require(poolSize > 0, "Pool size must be positive")
}

case class ConnectionConfig(
  initCommands: RedisBatch[Any] = RedisBatch.unit,
  debugListener: DebugListener = DevNullListener
)
