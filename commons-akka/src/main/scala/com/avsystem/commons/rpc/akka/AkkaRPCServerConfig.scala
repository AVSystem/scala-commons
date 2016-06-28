package com.avsystem.commons
package rpc.akka

import akka.actor.ActorPath

import scala.concurrent.duration._

/**
  * @author Wojciech Milewski
  */
final case class AkkaRPCServerConfig(
  actorName: String = "rpcServerActor",
  observableAckTimeout: FiniteDuration = 10.seconds
)

object AkkaRPCServerConfig {
  def default: AkkaRPCServerConfig = AkkaRPCServerConfig()
}

final case class AkkaRPCClientConfig(
  serverPath: ActorPath,
  observableMessageTimeout: FiniteDuration = 10.seconds,
  functionCallTimeout: FiniteDuration = 10.seconds
)