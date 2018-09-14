package com.avsystem.commons
package rpc

import com.avsystem.commons.meta._

/**
  * Mix in this trait into your RPC framework to support remote procedures, i.e. fire-and-forget methods
  * with `Unit` return type.
  */
trait ProcedureRPCFramework extends RPCFramework {
  type RawRPC <: ProcedureRawRPC

  trait ProcedureRawRPC { this: RawRPC =>
    @multi
    @verbatim def fire(@composite invocation: RawInvocation): Unit
  }

  case class ProcedureSignature(
    name: String,
    paramMetadata: List[ParamMetadata[_]],
    annotations: List[MetadataAnnotation]
  ) extends Signature with TypedMetadata[Unit]
}

/**
  * Mix in this trait into your RPC framework to support remote functions, i.e. methods which asynchronously
  * return some result (`Future[A]` where `A` has a `Reader` and `Writer`).
  */
trait FunctionRPCFramework extends RPCFramework {
  type RawRPC <: FunctionRawRPC

  trait FunctionRawRPC { this: RawRPC =>
    @multi def call(@composite invocation: RawInvocation): Future[RawValue]
  }

  case class FunctionSignature[T](
    name: String,
    paramMetadata: List[ParamMetadata[_]],
    annotations: List[MetadataAnnotation],
    @infer resultTypeMetadata: ResultTypeMetadata[T]
  ) extends Signature with TypedMetadata[Future[T]]

  implicit def readerBasedFutureAsReal[T: Reader]: AsReal[Future[RawValue], Future[T]] =
    AsReal.create(_.mapNow(read[T]))
  implicit def writerBasedFutureAsRaw[T: Writer]: AsRaw[Future[RawValue], Future[T]] =
    AsRaw.create(_.mapNow(write[T]))
}

/**
  * Mix in this trait into your RPC framework to support getters, i.e. methods that return RPC subinterfaces
  */
trait GetterRPCFramework extends RPCFramework {
  type RawRPC <: GetterRawRPC

  trait GetterRawRPC { this: RawRPC =>
    @multi def get(@composite invocation: RawInvocation): RawRPC

    def resolveGetterChain(getters: Seq[RawInvocation]): RawRPC =
      getters.foldRight(this)((inv, rpc) => rpc.get(inv))
  }

  case class GetterSignature[T](
    name: String,
    paramMetadata: List[ParamMetadata[_]],
    annotations: List[MetadataAnnotation],
    @infer @checked resultMetadata: RPCMetadata.Lazy[T]
  ) extends Signature with TypedMetadata[T]
}

trait StandardRPCFramework extends GetterRPCFramework with FunctionRPCFramework with ProcedureRPCFramework {
  trait RawRPC extends GetterRawRPC with FunctionRawRPC with ProcedureRawRPC
  object RawRPC extends BaseRawRpcCompanion

  trait FullRPCInfo[T] extends BaseFullRPCInfo[T]

  case class RPCMetadata[T](
    @reifyName name: String,
    @reifyAnnot @multi annotations: List[MetadataAnnotation],
    @multi @verbatim @rpcMethodMetadata procedureSignatures: Map[String, ProcedureSignature],
    @multi @rpcMethodMetadata functionSignatures: Map[String, FunctionSignature[_]],
    @multi @rpcMethodMetadata getterSignatures: Map[String, GetterSignature[_]]
  )
  object RPCMetadata extends RpcMetadataCompanion[RPCMetadata]
}

trait OneWayRPCFramework extends GetterRPCFramework with ProcedureRPCFramework {
  trait RawRPC extends GetterRawRPC with ProcedureRawRPC
  object RawRPC extends BaseRawRpcCompanion

  trait FullRPCInfo[T] extends BaseFullRPCInfo[T]

  case class RPCMetadata[T](
    @reifyName name: String,
    @reifyAnnot @multi annotations: List[MetadataAnnotation],
    @multi @verbatim @rpcMethodMetadata procedureSignatures: Map[String, ProcedureSignature],
    @multi @rpcMethodMetadata getterSignatures: Map[String, GetterSignature[_]]
  )
  object RPCMetadata extends RpcMetadataCompanion[RPCMetadata]
}
