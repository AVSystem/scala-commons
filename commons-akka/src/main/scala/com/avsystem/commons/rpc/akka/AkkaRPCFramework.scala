package com.avsystem.commons
package rpc.akka

import akka.actor.{ActorRef, ActorSystem}
import akka.routing.RoundRobinPool
import akka.stream.ActorMaterializer
import akka.util.ByteString
import com.avsystem.commons.rpc.akka.client.ClientRawRPC
import com.avsystem.commons.rpc.akka.serialization.{ByteStringInput, ByteStringOutput}
import com.avsystem.commons.rpc.akka.server.ServerActor
import com.avsystem.commons.rpc.{FunctionRPCFramework, GetterRPCFramework, MonifuRPCFramework, ProcedureRPCFramework}
import com.avsystem.commons.serialization.GenCodec

/**
  * @author Wojciech Milewski
  */
object AkkaRPCFramework extends GetterRPCFramework with ProcedureRPCFramework with FunctionRPCFramework with MonifuRPCFramework {
  trait RawRPC extends GetterRawRPC with ProcedureRawRPC with FunctionRawRPC with MonifuRawRPC

  override type RawValue = Array[Byte]

  override type Reader[T] = GenCodec[T]
  override type Writer[T] = GenCodec[T]

  override def read[T: Reader](raw: RawValue): T = GenCodec.read[T](new ByteStringInput(ByteString(raw)))
  override def write[T: Writer](value: T): RawValue = {
    val output = new ByteStringOutput
    GenCodec.write[T](output, value)
    output.result.toArray
  }

  def serverActor[T](rpc: T, config: AkkaRPCServerConfig = AkkaRPCServerConfig.default)(implicit system: ActorSystem, asRawRpc: AkkaRPCFramework.AsRawRPC[T]): ActorRef = {
    def roundRobin = RoundRobinPool(10).props(ServerActor.props(asRawRpc.asRaw(rpc)))
    def single = ServerActor.props(asRawRpc.asRaw(rpc))
    system.actorOf(single, config.actorName)
  }
  def client[T](config: AkkaRPCClientConfig)(implicit system: ActorSystem, materializer: ActorMaterializer, asRealRPC: AkkaRPCFramework.AsRealRPC[T]): T = {
    val rawRPC = new ClientRawRPC(config)
    asRealRPC.asReal(rawRPC)
  }
}
