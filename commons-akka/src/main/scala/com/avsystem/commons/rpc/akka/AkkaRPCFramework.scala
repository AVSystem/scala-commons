package com.avsystem.commons
package rpc.akka

import java.io.{DataInputStream, DataOutputStream}

import akka.actor.{ActorRef, ActorSystem}
import akka.util.ByteString
import com.avsystem.commons.rpc.akka.client.ClientRawRPC
import com.avsystem.commons.rpc.akka.server.ServerActor
import com.avsystem.commons.rpc.{FunctionRPCFramework, GetterRPCFramework, ProcedureRPCFramework, RpcMetadataCompanion, TypedMetadata, infer, multi, reify, reifyRpcName, verbatim}
import com.avsystem.commons.serialization.{GenCodec, StreamInput, StreamOutput}
import monix.reactive.Observable

/**
  * RPC Framework implemented with Akka as transportation layer.
  *
  * @author Wojciech Milewski
  */
object AkkaRPCFramework extends GetterRPCFramework with ProcedureRPCFramework with FunctionRPCFramework with MonixRPCFramework {
  trait RawRPC extends GetterRawRPC with ProcedureRawRPC with FunctionRawRPC with MonixRawRPC
  object RawRPC extends BaseRawRpcCompanion
  abstract class FullRPCInfo[T] extends BaseFullRPCInfo[T]

  type RawValue = ByteString

  type Reader[T] = GenCodec[T]
  type Writer[T] = GenCodec[T]
  type ParamTypeMetadata[T] = DummyImplicit
  type ResultTypeMetadata[T] = DummyImplicit

  case class RPCMetadata[T](
    @reifyRpcName name: String,
    @reify @multi annotations: List[MetadataAnnotation],
    @verbatim procedureSignatures: Map[String, ProcedureSignature],
    functionSignatures: Map[String, FunctionSignature[_]],
    observeSignatures: Map[String, ObserveSignature[_]],
    getterSignatures: Map[String, GetterSignature[_]]
  )
  object RPCMetadata extends RpcMetadataCompanion[RPCMetadata]

  case class ObserveSignature[T](
    name: String,
    paramMetadata: List[ParamMetadata[_]],
    annotations: List[MetadataAnnotation],
    @infer resultTypeMetadata: ResultTypeMetadata[T]
  ) extends Signature with TypedMetadata[Observable[T]]

  def read[T: Reader](raw: RawValue): T =
    GenCodec.read[T](new StreamInput(new DataInputStream(raw.iterator.asInputStream)))
  def write[T: Writer](value: T): RawValue = {
    val builder = ByteString.newBuilder
    val output = new StreamOutput(new DataOutputStream(builder.asOutputStream))
    GenCodec.write[T](output, value)
    builder.result()
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
