package com.avsystem.commons
package rpc

import com.avsystem.commons.meta.*
import com.avsystem.commons.misc.TypeString
import com.avsystem.commons.rpc.DummyRPC.{*, given}
import com.avsystem.commons.serialization.GenCodec
import com.avsystem.commons.serialization.json.{JsonStringInput, JsonStringOutput}

import scala.annotation.nowarn

class namedArgs extends StaticAnnotation

object DummyRPC {
  type AsRawRPC[T] = RawRPC.AsRawRpc[T]
  type AsRealRPC[T] = RawRPC.AsRealRpc[T]
  type AsRawRealRPC[T] = RawRPC.AsRawRealRpc[T]
  def read[T: GenCodec](raw: String): T = JsonStringInput.read[T](raw)
  def write[T: GenCodec](value: T): String = JsonStringOutput.write[T](value)
  given [T: GenCodec] => AsRawReal[String, T] = AsRawReal.create(write[T], read[T])
  given [T: GenCodec] => AsReal[Future[String], Future[T]] = _.mapNow(read[T])
  given [T: GenCodec] => AsRaw[Future[String], Future[T]] = _.mapNow(write[T])
  given [T: Tag] => (tag: Tag[T]) => GenCodec[T] = tag.codec
  trait RawRPC {
    @multi
    @verbatim
    @annotated[namedArgs]
    def fireNamed(@composite invocation: RawNamedInvocation): Unit

    @multi
    @verbatim
    def fire(@composite invocation: RawInvocation): Unit

    @multi
    @encoded
    def call(@composite invocation: RawInvocation): Future[String]

    @multi
    @encoded
    def get(@composite invocation: RawInvocation): RawRPC

    def resolveGetterChain(getters: Seq[RawInvocation]): RawRPC =
      getters.foldRight(this)((inv, rpc) => rpc.get(inv))
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
  abstract class RPCCompanion[T](
//    using instances: MacroInstances[DummyRPC.type, (asRaw: AsRawRPC[T], asReal: AsRealRPC[T], metadata: RPCMetadata[T])],
  ) {
    given asRawRPC: AsRawRPC[T] = ???
//      instances(DummyRPC, this).asRaw
    given asRealRPC: AsRealRPC[T] = ???
//    nstances(DummyRPC, this).asReal
    given metadata: RPCMetadata[T] = ???
//    instances(DummyRPC, this).metadata
  }
  case class RawInvocation(@methodName rpcName: String, @multi args: List[String])
  case class RawNamedInvocation(@methodName rpcName: String, @multi args: IListMap[String, String])
  case class ParamMetadata[T](
    @reifyName name: String,
    @reifyAnnot @multi annotations: List[MetadataAnnotation],
    @infer typeMetadata: TypeString[T],
  ) extends ParamSignature
      with TypedMetadata[T]
  case class GenericParamMetadata[T](
    @reifyName name: String,
    @reifyAnnot @multi annotations: List[MetadataAnnotation],
    @forTypeParams @infer typeMetadata: List[TypeString[?]] => TypeString[T],
  ) extends ParamSignature
      with TypedMetadata[T]
  case class TypeParamMetadata(
    @reifyName name: String,
  )
  case class ProcedureSignature(
    @reifyName name: String,
    @multi @rpcParamMetadata paramMetadata: List[ParamMetadata[?]],
    @reifyAnnot @multi annotations: List[MetadataAnnotation],
  ) extends Signature
      with TypedMetadata[Unit]
  case class FunctionSignature[T](
    @reifyName name: String,
    @multi @rpcTypeParamMetadata typeParamMetadata: List[TypeParamMetadata],
    @multi @rpcParamMetadata paramMetadata: List[GenericParamMetadata[?]],
    @reifyAnnot @multi annotations: List[MetadataAnnotation],
    @forTypeParams @infer resultTypeMetadata: List[ClassTag[?]] => ClassTag[T],
  ) extends Signature
      with TypedMetadata[Future[T]]
  case class GetterSignature[T](
    @reifyName name: String,
    @multi @rpcParamMetadata paramMetadata: List[ParamMetadata[?]],
    @reifyAnnot @multi annotations: List[MetadataAnnotation],
    @infer @checked resultMetadata: RPCMetadata.Lazy[T],
  ) extends Signature
      with TypedMetadata[T]
  case class RPCMetadata[T](
    @reifyName name: String,
    @reifyAnnot @multi annotations: List[MetadataAnnotation],
    @multi @verbatim @rpcMethodMetadata procedureSignatures: Map[String, ProcedureSignature],
    @multi @rpcMethodMetadata functionSignatures: Map[String, FunctionSignature[?]],
    @multi @rpcMethodMetadata getterSignatures: Map[String, GetterSignature[?]],
  )
  object RawRPC extends RawRpcCompanion[RawRPC]
  object AsRawRPC {
    def apply[T](using asRawRPC: AsRawRPC[T]): AsRawRPC[T] = asRawRPC
  }
  object AsRealRPC {
    def apply[T](using asRealRPC: AsRealRPC[T]): AsRealRPC[T] = asRealRPC
  }
  object AsRawRealRPC {
    def apply[T](using asRawRealRPC: AsRawRealRPC[T]): AsRawRealRPC[T] = asRawRealRPC
  }
  object RPCMetadata extends RpcMetadataCompanion[RPCMetadata]

}
