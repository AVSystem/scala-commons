package com.avsystem.commons
package rpc.akka

import akka.actor.ActorPath

import scala.concurrent.duration._

/**
  * @author Wojciech Milewski
  */
final case class AkkaRPCServerConfig(
  actorName: String = "rpcServerActor",
  observableAckTimeout: FiniteDuration = 10.seconds,
  /**
    * Heartbeat interval indicates how frequent hearbeat will be sent. Interval should be several times lower
    * than observableMessageTimeout from [[AkkaRPCClientConfig]] to be sure that heartbeat will be delivered during
    * timeout when server is connected.
    */
  heartbeatInterval: FiniteDuration = 3.seconds
)

object AkkaRPCServerConfig {
  def default: AkkaRPCServerConfig = AkkaRPCServerConfig()
}

final case class AkkaRPCClientConfig(
  serverPath: ActorPath,
  observableMessageTimeout: FiniteDuration = 10.seconds,
  functionCallTimeout: FiniteDuration = 10.seconds
)