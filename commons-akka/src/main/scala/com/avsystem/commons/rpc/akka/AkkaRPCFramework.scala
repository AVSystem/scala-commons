package com.avsystem.commons
package rpc.akka

import akka.actor.{ActorRef, ActorSystem}
import akka.util.ByteString
import com.avsystem.commons.rpc.akka.client.ClientRawRPC
import com.avsystem.commons.rpc.akka.serialization.{ByteStringLinearInput, ByteStringLinearOutput}
import com.avsystem.commons.rpc.akka.server.ServerActor
import com.avsystem.commons.rpc.{FunctionRPCFramework, GetterRPCFramework, ProcedureRPCFramework}
import com.avsystem.commons.serialization.GenCodec

/**
  * RPC Framework implemented with Akka as transportation layer.
  *
  * @author Wojciech Milewski
  */
object AkkaRPCFramework extends GetterRPCFramework with ProcedureRPCFramework with FunctionRPCFramework with MonixRPCFramework {
  trait RawRPC extends GetterRawRPC with ProcedureRawRPC with FunctionRawRPC with MonixRawRPC

  override type RawValue = ByteString

  override type Reader[T] = GenCodec[T]
  override type Writer[T] = GenCodec[T]

  override def read[T: Reader](raw: RawValue): T = GenCodec.read[T](new ByteStringLinearInput(raw))
  override def write[T: Writer](value: T): RawValue = {
    val output = new ByteStringLinearOutput(ByteString.newBuilder)
    GenCodec.write[T](output, value)
    output.result
  }

  /**
    * Creates server actor which listens for incoming requests.
    *
    * To stop server from listening, kill an actor returned from the method.
    *
    * @param rpc    actual implementation of `T` RPC
    * @param config contains configuration on created actor name, timeouts etc.
    * @tparam T type of RPC
    */
  def serverActor[T](rpc: T, config: AkkaRPCServerConfig = AkkaRPCServerConfig.default)(implicit system: ActorSystem, asRawRpc: AkkaRPCFramework.AsRawRPC[T]): ActorRef = {
    system.actorOf(ServerActor.props(asRawRpc.asRaw(rpc), config), config.actorName)
  }

  /**
    * Returns client RPC of type `T`. Each method call on returned object will make a remote call.
    */
  def client[T](config: AkkaRPCClientConfig)(implicit system: ActorSystem, asRealRPC: AkkaRPCFramework.AsRealRPC[T]): T = {
    val rawRPC = new ClientRawRPC(config)
    asRealRPC.asReal(rawRPC)
  }
}
