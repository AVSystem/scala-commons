package com.avsystem.commons
package redis.monitoring

import com.avsystem.commons.misc.{AbstractCase, AbstractNamedEnumCompanion, AutoNamedEnum}
import com.avsystem.commons.redis.NodeAddress

trait ConnectionStateObserver {
  def onConnectionStateChange(addr: NodeAddress, state: ConnectionState): Unit
}

sealed abstract class ConnectionState extends AbstractCase with AutoNamedEnum

object ConnectionState extends AbstractNamedEnumCompanion[ConnectionState]{
  case object Created extends ConnectionState
  case object Connecting extends ConnectionState
  case object Connected extends ConnectionState
  case object Closed extends ConnectionState
  case object Removed extends ConnectionState

  override val values: Seq[ConnectionState] = caseObjects
}
