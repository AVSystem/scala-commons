package com.avsystem.commons
package rpc

import scala.language.higherKinds

trait RPCFramework {
  type RawValue
  type Reader[T]
  type Writer[T]

  type RawRPC
  val RawRPC: BaseRawRpcCompanion

  trait BaseRawRpcCompanion extends RawRpcCompanion[RawRPC] {
    override val implicits: RPCFramework.this.type = RPCFramework.this
  }

  def read[T: Reader](raw: RawValue): T
  def write[T: Writer](value: T): RawValue

  implicit def readerBasedAsReal[T: Reader]: AsReal[RawValue, T] = AsReal.create(read[T])
  implicit def writerBasedAsRaw[T: Writer]: AsRaw[RawValue, T] = AsRaw.create(write[T])

  type ParamTypeMetadata[T]
  type ResultTypeMetadata[T]

  type RPCMetadata[T]
  val RPCMetadata: RpcMetadataCompanion[RPCMetadata]

  type AsRawRPC[T] = AsRaw[RawRPC, T]
  object AsRawRPC {
    def apply[T](implicit asRawRPC: AsRawRPC[T]): AsRawRPC[T] = asRawRPC
  }

  /**
    * Materializes a factory of implementations of [[RawRPC]] which translate invocations of its raw methods
    * to invocations of actual methods on `rpcImpl`. Method arguments and results are serialized and deserialized
    * from/to [[RawValue]] using [[Reader]] and [[Writer]] typeclasses.
    */
  def materializeAsRaw[T]: AsRawRPC[T] = macro macros.rpc.RPCFrameworkMacros.asRawImpl[T]

  type AsRealRPC[T] = AsReal[RawRPC, T]
  object AsRealRPC {
    @inline def apply[T](implicit asRealRPC: AsRealRPC[T]): AsRealRPC[T] = asRealRPC
  }

  /**
    * Materializes a factory of implementations of `T` which are proxies that implement all abstract methods of `T`
    * by forwarding them to `rawRpc`. Method arguments and results are serialized and deserialized
    * from/to [[RawValue]] using [[Reader]] and [[Writer]] typeclasses.
    */
  def materializeAsReal[T]: AsRealRPC[T] = macro macros.rpc.RPCFrameworkMacros.asRealImpl[T]

  type AsRealRawRPC[T] = AsRealRaw[RawRPC, T]
  object AsRealRawRPC {
    @inline def apply[T](implicit asRealRawRPC: AsRealRawRPC[T]): AsRealRawRPC[T] = asRealRawRPC
  }

  def materializeAsRealRaw[T]: AsRealRawRPC[T] = macro macros.rpc.RPCFrameworkMacros.asRealRawImpl[T]

  trait Signature {
    @reifyName def name: String
    @multi def paramMetadata: List[ParamMetadata[_]]
    @reify
    @multi def annotations: List[MetadataAnnotation]
  }

  case class ParamMetadata[T](
    @reifyName name: String,
    @reify @multi annotations: List[MetadataAnnotation],
    @infer typeMetadata: ParamTypeMetadata[T]
  ) extends TypedMetadata[T]

  def materializeMetadata[T]: RPCMetadata[T] = macro macros.rpc.RPCFrameworkMacros.metadataImpl[T]

  /**
    * Base trait for traits or classes "implementing" [[FullRPCInfo]] in various RPC frameworks.
    * Having a separate subtrait/subclass for every framework is beneficial for ScalaJS DCE.
    */
  trait BaseFullRPCInfo[T] {
    def asRealRPC: AsRealRPC[T]
    def asRawRPC: AsRawRPC[T]
    def metadata: RPCMetadata[T]
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
    *   abstract class FullRPCInfo[T] extends BaseFullRPCInfo[T]
    *   ...
    * }
    * }}}
    */
  type FullRPCInfo[T] <: BaseFullRPCInfo[T]

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
  abstract class RPCCompanion[T](implicit fri: FullRPCInfo[T]) {
    final def fullRpcInfo: FullRPCInfo[T] = fri
    // You would think: why the hell are these implicits defined as macros?
    // Can't we just simply refer to members of `fullRpcInfo` in a regular method?
    // We can, but this prevents ScalaJS optimizer's DCE from distinguishing between `FullRPCInfo` traits/classes
    // of different RPC frameworks. This is important in cross-compiled code where any of these three typeclasses
    // may be completely unused on the JS side and we want to make sure that DCE gets rid of them.
    implicit def asRealRPC: AsRealRPC[T] = macro macros.rpc.RPCFrameworkMacros.typeClassFromFullInfo
    implicit def asRawRPC: AsRawRPC[T] = macro macros.rpc.RPCFrameworkMacros.typeClassFromFullInfo
    implicit def metadata: RPCMetadata[T] = macro macros.rpc.RPCFrameworkMacros.typeClassFromFullInfo
  }
}
