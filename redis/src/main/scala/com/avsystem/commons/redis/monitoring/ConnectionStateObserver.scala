package com.avsystem.commons
package redis.monitoring

import com.avsystem.commons.misc.{AbstractValueEnum, AbstractValueEnumCompanion, EnumCtx}
import com.avsystem.commons.redis.NodeAddress

/** Intended for monitoring the state of a single Redis connection. Should be non-blocking and handle internal
  * exceptions by itself.
  */
trait ConnectionStateObserver {
  def onConnectionStateChange(addr: NodeAddress, state: ConnectionState): Unit
}

final class ConnectionState(implicit enumCtx: EnumCtx) extends AbstractValueEnum

object ConnectionState extends AbstractValueEnumCompanion[ConnectionState] {
  final val Created, Connecting, Connected, Closed, Removed: ConnectionState = new ConnectionState
}
