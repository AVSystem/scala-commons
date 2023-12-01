package com.avsystem.commons
package redis.monitoring

/**
  * Intended for monitoring client's connections.
  * Should be non-blocking and handle internal exceptions by itself.
  */
trait ClusterStateObserver {
  def onConnectionInitFailure(addr: String): Unit
  def onOpenedConnection(addr: String): Unit
  def onSeedNodeFailure(): Unit
  def onClusterInitFailure(): Unit
  def onRemovedMaster(addr: String): Unit
}
