package com.avsystem.commons
package rpc

import com.avsystem.commons.meta._
import com.avsystem.commons.serialization.GenCodec

trait RPCFramework {
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

  /**
    * Materializes a factory of implementations of [[RawRPC]] which translate invocations of its raw methods
    * to invocations of actual methods on `rpcImpl`. Method arguments and results are serialized and deserialized
    * from/to [[RawValue]] using [[Reader]] and [[Writer]] typeclasses.
    */
  def materializeAsRaw[T]: AsRawRPC[T] = macro macros.rpc.RPCFrameworkMacros.asRawImpl[T]

  type AsRealRPC[RealRPC] = AsReal[RawRPC, RealRPC]
  object AsRealRPC {
    @inline def apply[T](implicit asRealRPC: AsRealRPC[T]): AsRealRPC[T] = asRealRPC
  }

  /**
    * Materializes a factory of implementations of `T` which are proxies that implement all abstract methods of `T`
    * by forwarding them to `rawRpc`. Method arguments and results are serialized and deserialized
    * from/to [[RawValue]] using [[Reader]] and [[Writer]] typeclasses.
    */
  def materializeAsReal[T]: AsRealRPC[T] = macro macros.rpc.RPCFrameworkMacros.asRealImpl[T]

  type AsRawRealRPC[RealRPC] = AsRawReal[RawRPC, RealRPC]
  object AsRawRealRPC {
    @inline def apply[RealRPC](implicit AsRawRealRPC: AsRawRealRPC[RealRPC]): AsRawRealRPC[RealRPC] = AsRawRealRPC
  }

  def materializeAsRawReal[T]: AsRawRealRPC[T] = macro macros.rpc.RPCFrameworkMacros.AsRawRealImpl[T]

  trait Signature {
    @reifyName def name: String
    @multi @rpcParamMetadata def paramMetadata: List[ParamMetadata[_]]
    @reifyAnnot
    @multi def annotations: List[MetadataAnnotation]
  }

  case class ParamMetadata[T](
    @reifyName name: String,
    @reifyAnnot @multi annotations: List[MetadataAnnotation],
    @infer typeMetadata: ParamTypeMetadata[T]
  ) extends TypedMetadata[T]

  def materializeMetadata[RealRPC]: RPCMetadata[RealRPC] = macro macros.rpc.RPCFrameworkMacros.metadataImpl[RealRPC]

  /**
    * Base trait for traits or classes "implementing" [[FullRPCInfo]] in various RPC frameworks.
    * Having a separate subtrait/subclass for every framework is beneficial for ScalaJS DCE.
    */
  trait BaseFullRPCInfo[RealRPC] {
    def asRealRPC: AsRealRPC[RealRPC]
    def asRawRPC: AsRawRPC[RealRPC]
    def metadata: RPCMetadata[RealRPC]
  }
  /**
    * This type must be defined as trait or class by an [[RPCFramework]] in order to be able
    * to use it's [[RPCCompanion]]. The fact that every [[RPCFramework]] may define its own trait or class for
    * [[FullRPCInfo]] helps ScalaJS DCE distinguish between instances of [[AsRawRPC]], [[AsRealRPC]] and [[RPCMetadata]]
    * for different frameworks and to get rid of unused instances.
    *
    * @example
    * {{{
    * object SomeRPCFramework extends RPCFramework {
    *   abstract class FullRPCInfo[RealRPC] extends BaseFullRPCInfo[RealRPC]
    *   ...
    * }
    * }}}
    */
  type FullRPCInfo[RealRPC] <: BaseFullRPCInfo[RealRPC]

  implicit def materializeFullInfo[T]: FullRPCInfo[T] = macro macros.rpc.RPCFrameworkMacros.fullInfoImpl[T]

  /**
    * Convenience abstract class for companion objects of RPC interfaces. Makes sure all three RPC type classes
    * ([[AsRawRPC]], [[AsRealRPC]] and [[RPCMetadata]]) are macro-materialized for that RPC interface and confines
    * macro materialization to the same compilation unit where the RPC interface is defined.
    * This is a good practice to avoid incremental compilation problems and duplication of macro-generated code
    * in various callsites. In order to be able to use [[RPCCompanion]], the RPC framework must define [[FullRPCInfo]]
    * as a trait or class. Additionally, some special wizardry has been employed to make sure that when an RPC interface
    * is a part of shared (cross-compiled) code of a ScalaJS application then ScalaJS optimizer can remove unused
    * instances of macro generated typeclasses.
    *
    * @example
    * {{{
    *   object SomeRPCFramework extends StandardRPCFramework { ... }
    *   @RPC trait SomeRPC {
    *     def doSomething(str: String): Unit
    *     def callSomething(int: Int): Future[String]
    *   }
    *   object SomeRPC extends SomeRPCFramework.RPCCompanion[SomeRPC]
    * }}}
    */
  abstract class RPCCompanion[RealRPC](implicit fri: FullRPCInfo[RealRPC]) {
    final def fullRpcInfo: FullRPCInfo[RealRPC] = fri
    // You would think: why the hell are these implicits defined as macros?
    // Can't we just simply refer to members of `fullRpcInfo` in a regular method?
    // We can, but this prevents ScalaJS optimizer's DCE from distinguishing between `FullRPCInfo` traits/classes
    // of different RPC frameworks. This is important in cross-compiled code where any of these three typeclasses
    // may be completely unused on the JS side and we want to make sure that DCE gets rid of them.
    implicit def asRealRPC: AsRealRPC[RealRPC] = macro macros.rpc.RPCFrameworkMacros.typeClassFromFullInfo
    implicit def asRawRPC: AsRawRPC[RealRPC] = macro macros.rpc.RPCFrameworkMacros.typeClassFromFullInfo
    implicit def metadata: RPCMetadata[RealRPC] = macro macros.rpc.RPCFrameworkMacros.typeClassFromFullInfo
  }
}
