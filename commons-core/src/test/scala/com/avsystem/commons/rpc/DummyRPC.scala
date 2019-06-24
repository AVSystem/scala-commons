package com.avsystem.commons
package rpc

import com.avsystem.commons.meta.{MacroInstances, TypedMetadata, checked, composite, infer, multi, reifyAnnot, reifyName}
import com.avsystem.commons.misc.TypeString
import com.avsystem.commons.serialization.GenCodec
import com.avsystem.commons.serialization.json.{JsonStringInput, JsonStringOutput}

object DummyRPC {
  case class RawInvocation(@methodName rpcName: String, @multi args: List[String])

  trait RawRPC {
    @multi @verbatim def
    fire(@composite invocation: RawInvocation): Unit

    @multi @encoded
    def call(@composite invocation: RawInvocation): Future[String]

    @multi @encoded
    def get(@composite invocation: RawInvocation): RawRPC

    def resolveGetterChain(getters: Seq[RawInvocation]): RawRPC =
      getters.foldRight(this)((inv, rpc) => rpc.get(inv))
  }
  object RawRPC extends RawRpcCompanion[RawRPC]

  type AsRawRPC[T] = RawRPC.AsRawRpc[T]
  object AsRawRPC {
    def apply[T](implicit asRawRPC: AsRawRPC[T]): AsRawRPC[T] = asRawRPC
  }

  type AsRealRPC[T] = RawRPC.AsRealRpc[T]
  object AsRealRPC {
    def apply[T](implicit asRealRPC: AsRealRPC[T]): AsRealRPC[T] = asRealRPC
  }

  type AsRawRealRPC[T] = RawRPC.AsRawRealRpc[T]
  object AsRawRealRPC {
    def apply[T](implicit asRawRealRPC: AsRawRealRPC[T]): AsRawRealRPC[T] = asRawRealRPC
  }

  trait Signature {
    def name: String
    def annotations: List[MetadataAnnotation]
    def paramMetadata: List[ParamSignature]
  }

  trait ParamSignature {
    def name: String
    def annotations: List[MetadataAnnotation]
  }

  case class ParamMetadata[T](
    @reifyName name: String,
    @reifyAnnot @multi annotations: List[MetadataAnnotation],
    @infer typeMetadata: TypeString[T]
  ) extends ParamSignature with TypedMetadata[T]

  case class GenericParamMetadata[T](
    @reifyName name: String,
    @reifyAnnot @multi annotations: List[MetadataAnnotation],
    @forTypeParams @infer typeMetadata: List[TypeString[_]] => TypeString[T]
  ) extends ParamSignature with TypedMetadata[T]

  case class TypeParamMetadata(
    @reifyName name: String
  )

  case class ProcedureSignature(
    @reifyName name: String,
    @multi @rpcParamMetadata paramMetadata: List[ParamMetadata[_]],
    @reifyAnnot @multi annotations: List[MetadataAnnotation]
  ) extends Signature with TypedMetadata[Unit]

  case class FunctionSignature[T](
    @reifyName name: String,
    @multi @rpcTypeParamMetadata typeParamMetadata: List[TypeParamMetadata],
    @multi @rpcParamMetadata paramMetadata: List[GenericParamMetadata[_]],
    @reifyAnnot @multi annotations: List[MetadataAnnotation],
    @forTypeParams @infer resultTypeMetadata: List[ClassTag[_]] => ClassTag[T]
  ) extends Signature with TypedMetadata[Future[T]]

  case class GetterSignature[T](
    @reifyName name: String,
    @multi @rpcParamMetadata paramMetadata: List[ParamMetadata[_]],
    @reifyAnnot @multi annotations: List[MetadataAnnotation],
    @infer @checked resultMetadata: RPCMetadata.Lazy[T]
  ) extends Signature with TypedMetadata[T]

  case class RPCMetadata[T](
    @reifyName name: String,
    @reifyAnnot @multi annotations: List[MetadataAnnotation],
    @multi @verbatim @rpcMethodMetadata procedureSignatures: Map[String, ProcedureSignature],
    @multi @rpcMethodMetadata functionSignatures: Map[String, FunctionSignature[_]],
    @multi @rpcMethodMetadata getterSignatures: Map[String, GetterSignature[_]]
  )
  object RPCMetadata extends RpcMetadataCompanion[RPCMetadata]

  def read[T: GenCodec](raw: String): T = JsonStringInput.read[T](raw)
  def write[T: GenCodec](value: T): String = JsonStringOutput.write[T](value)

  implicit def anyAsRawReal[T: GenCodec]: AsRawReal[String, T] =
    AsRawReal.create(write[T], read[T])

  implicit def readerBasedFutureAsReal[T: GenCodec]: AsReal[Future[String], Future[T]] = _.mapNow(read[T])
  implicit def writerBasedFutureAsRaw[T: GenCodec]: AsRaw[Future[String], Future[T]] = _.mapNow(write[T])

  implicit def codecFromTag[T](implicit tag: Tag[T]): GenCodec[T] = tag.codec

  trait Instances[T] {
    def asRaw: AsRawRPC[T]
    def asReal: AsRealRPC[T]
    def metadata: RPCMetadata[T]
  }

  abstract class RPCCompanion[T](implicit instances: MacroInstances[DummyRPC.type, Instances[T]]) {
    implicit lazy val asRawRPC: AsRawRPC[T] = instances(DummyRPC, this).asRaw
    implicit lazy val asRealRPC: AsRealRPC[T] = instances(DummyRPC, this).asReal
    implicit lazy val metadata: RPCMetadata[T] = instances(DummyRPC, this).metadata
  }
}