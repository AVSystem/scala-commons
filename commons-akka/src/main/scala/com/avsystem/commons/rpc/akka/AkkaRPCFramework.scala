package com.avsystem.commons
package rpc.akka

import akka.actor.{ActorPath, ActorRef, ActorSystem}
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

  def serverActor[T](rpc: T)(implicit system: ActorSystem, asRawRpc: AkkaRPCFramework.AsRawRPC[T]): ActorRef = {
    system.actorOf(ServerActor.props(asRawRpc.asRaw(rpc)), "rpcServerActor")    //todo load name from config
  }
  def client[T](serverActorPath: ActorPath)(implicit system: ActorSystem, materializer: ActorMaterializer, asRealRPC: AkkaRPCFramework.AsRealRPC[T]): T = {
    val procedureClientActor = system.actorOf(ProcedureClientActor.props(serverActorPath))
    val functionClientActor = system.actorOf(FunctionClientActor.props(serverActorPath))
    val rawRPC = new ClientRawRPC(procedureClientActor, functionClientActor, serverActorPath)
    asRealRPC.asReal(rawRPC)
  }
}
