package com.avsystem.commons
package rpc.akka

import akka.actor.{ActorRef, ActorSystem}
import akka.routing.RoundRobinPool
import akka.stream.ActorMaterializer
import com.avsystem.commons.rpc.akka.client.{ClientRawRPC, FunctionClientActor, ProcedureClientActor}
import com.avsystem.commons.rpc.akka.server.ServerActor
import com.avsystem.commons.rpc.{FunctionRPCFramework, GetterRPCFramework, MonifuRPCFramework, ProcedureRPCFramework}

/**
  * @author Wojciech Milewski
  */
object AkkaRPCFramework extends GetterRPCFramework with ProcedureRPCFramework with FunctionRPCFramework with MonifuRPCFramework {
  trait RawRPC extends GetterRawRPC with ProcedureRawRPC with FunctionRawRPC with MonifuRawRPC

  override type RawValue = Any

  override type Reader[T] = DummyImplicit
  override type Writer[T] = DummyImplicit

  override def read[T: Reader](raw: RawValue): T = raw.asInstanceOf[T]
  override def write[T: Writer](value: T): RawValue = value

  def serverActor[T](rpc: T, config: AkkaRPCServerConfig = AkkaRPCServerConfig.default)(implicit system: ActorSystem, asRawRpc: AkkaRPCFramework.AsRawRPC[T]): ActorRef = {
    def roundRobin = RoundRobinPool(10).props(ServerActor.props(asRawRpc.asRaw(rpc)))
    def single = ServerActor.props(asRawRpc.asRaw(rpc))
    system.actorOf(single, config.actorName)
  }
  def client[T](config: AkkaRPCClientConfig)(implicit system: ActorSystem, materializer: ActorMaterializer, asRealRPC: AkkaRPCFramework.AsRealRPC[T]): T = {
    val procedureClientActor = system.actorOf(ProcedureClientActor.props(config.serverPath))
    val functionClientActor = system.actorOf(FunctionClientActor.props(config))
    val rawRPC = new ClientRawRPC(procedureClientActor, functionClientActor, config)
    asRealRPC.asReal(rawRPC)
  }
}
