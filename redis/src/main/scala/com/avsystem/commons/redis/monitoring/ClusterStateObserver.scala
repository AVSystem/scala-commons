package com.avsystem.commons
package redis.monitoring

import com.avsystem.commons.redis.RedisClusterClient

/** Intended for monitoring [[RedisClusterClient]]'s connections. Should be non-blocking and handle internal exceptions
  * by itself.
  */
trait ClusterStateObserver extends ConnectionStateObserver {
  def onClusterRefresh(): Unit
  def onClusterRefreshFailure(): Unit
  def onClusterInitialized(): Unit
  def onClusterInitFailure(): Unit
}
