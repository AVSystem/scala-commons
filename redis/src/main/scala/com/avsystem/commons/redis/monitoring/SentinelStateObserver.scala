package com.avsystem.commons
package redis.monitoring

import com.avsystem.commons.redis.NodeAddress
import com.avsystem.commons.redis.RedisMasterSlaveClient

/**
  * Intended for monitoring [[RedisMasterSlaveClient]]'s state and connections.
  * Should be non-blocking and handle internal exceptions by itself.
  */
trait SentinelStateObserver extends ConnectionStateObserver {
  def onMasterChange(master: NodeAddress): Unit
}
