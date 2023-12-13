package com.avsystem.commons
package redis.monitoring

import com.avsystem.commons.redis.RedisNodeClient

trait SentinelStateObserver extends ConnectionStateObserver {
  def onMasterChange(master: RedisNodeClient): Unit
}
