package com.avsystem.commons
package redis.monitoring

/**
  * Intended for monitoring client's connections.
  * Should be non-blocking and handle internal exceptions by itself.
  */
trait ClusterStateObserver extends ConnectionStateObserver {
  def onClusterRefresh(): Unit
  def onClusterRefreshFailure(): Unit
  def onClusterInitFailure(): Unit
}

