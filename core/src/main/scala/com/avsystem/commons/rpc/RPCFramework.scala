package com.avsystem.commons
package rpc

import com.avsystem.commons.meta.*
import com.avsystem.commons.serialization.GenCodec

trait RPCFramework extends RPCFrameworkMacros {
  type RawValue
  type Reader[T]
  type Writer[T]

  case class RawInvocation(@methodName rpcName: String, @multi args: List[RawValue])
  object RawInvocation {
    implicit def codec(implicit rawValueCodec: GenCodec[RawValue]): GenCodec[RawInvocation] = GenCodec.materialize
  }

  type RawRPC
  val RawRPC: BaseRawRpcCompanion

  trait BaseRawRpcCompanion extends RawRpcCompanion[RawRPC]

  def read[T: Reader](raw: RawValue): T
  def write[T: Writer](value: T): RawValue

  implicit def readerBasedAsReal[T: Reader]: AsReal[RawValue, T] = read[T](_)
  implicit def writerBasedAsRaw[T: Writer]: AsRaw[RawValue, T] = write[T](_)

  type ParamTypeMetadata[T]
  type ResultTypeMetadata[T]

  type RPCMetadata[RealRPC]
  val RPCMetadata: RpcMetadataCompanion[RPCMetadata]

  type AsRawRPC[RealRPC] = AsRaw[RawRPC, RealRPC]
  object AsRawRPC {
    def apply[RealRPC](implicit asRawRPC: AsRawRPC[RealRPC]): AsRawRPC[RealRPC] = asRawRPC
  }

  type AsRealRPC[RealRPC] = AsReal[RawRPC, RealRPC]
  object AsRealRPC {
    @inline def apply[T](implicit asRealRPC: AsRealRPC[T]): AsRealRPC[T] = asRealRPC
  }

  type AsRawRealRPC[RealRPC] = AsRawReal[RawRPC, RealRPC]
  object AsRawRealRPC {
    @inline def apply[RealRPC](implicit AsRawRealRPC: AsRawRealRPC[RealRPC]): AsRawRealRPC[RealRPC] = AsRawRealRPC
  }

  trait Signature {
    @reifyName def name: String
    @multi
    @rpcParamMetadata def paramMetadata: List[ParamMetadata[?]]
    @reifyAnnot
    @multi def annotations: List[MetadataAnnotation]
  }

  case class ParamMetadata[T](
    @reifyName name: String,
    @reifyAnnot @multi annotations: List[MetadataAnnotation],
    @infer typeMetadata: ParamTypeMetadata[T],
  ) extends TypedMetadata[T]

  /**
   * Base trait for traits or classes "implementing" [[FullRPCInfo]] in various RPC frameworks. Having a separate
   * subtrait/subclass for every framework is beneficial for ScalaJS DCE.
   */
  trait BaseFullRPCInfo[RealRPC] {
    def asRealRPC: AsRealRPC[RealRPC]
    def asRawRPC: AsRawRPC[RealRPC]
    def metadata: RPCMetadata[RealRPC]
  }

  /**
   * This type must be defined as trait or class by an [[RPCFramework]] in order to be able to use it's
   * [[RPCCompanion]]. The fact that every [[RPCFramework]] may define its own trait or class for [[FullRPCInfo]] helps
   * ScalaJS DCE distinguish between instances of [[AsRawRPC]], [[AsRealRPC]] and [[RPCMetadata]] for different
   * frameworks and to get rid of unused instances.
   */
  type FullRPCInfo[RealRPC] <: BaseFullRPCInfo[RealRPC]

  /**
   * Convenience abstract class for companion objects of RPC interfaces. Makes sure all three RPC type classes
   * ([[AsRawRPC]], [[AsRealRPC]] and [[RPCMetadata]]) are macro-materialized for that RPC interface and confines macro
   * materialization to the same compilation unit where the RPC interface is defined. This is a good practice to avoid
   * incremental compilation problems and duplication of macro-generated code in various callsites. In order to be able
   * to use [[RPCCompanion]], the RPC framework must define [[FullRPCInfo]] as a trait or class. Additionally, some
   * special wizardry has been employed to make sure that when an RPC interface is a part of shared (cross-compiled)
   * code of a ScalaJS application then ScalaJS optimizer can remove unused instances of macro generated typeclasses.
   */
  abstract class RPCCompanion[RealRPC](implicit fri: FullRPCInfo[RealRPC]) extends FullRPCInfoMacros {
    final def fullRpcInfo: FullRPCInfo[RealRPC] = fri
  }
}
