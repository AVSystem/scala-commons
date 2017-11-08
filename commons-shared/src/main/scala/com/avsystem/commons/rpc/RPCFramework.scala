package com.avsystem.commons
package rpc

import scala.annotation.implicitNotFound
import scala.language.higherKinds

trait RPCFramework {
  type RawValue
  type Reader[T]
  type Writer[T]
  type RawRPC

  def read[T: Reader](raw: RawValue): T
  def write[T: Writer](value: T): RawValue

  type ParamTypeMetadata[T]
  type ResultTypeMetadata[T]

  object RPCMetadata {
    @inline def apply[T](implicit metadata: RPCMetadata[T]): RPCMetadata[T] = metadata
  }

  @implicitNotFound("This RPC framework doesn't support RPC methods that return ${Real} " +
    "or some implicit dependencies may be missing (e.g. Writer[A] when result type is Future[A])")
  trait RealInvocationHandler[Real, Raw] {
    def toRaw(real: Real): Raw
  }
  object RealInvocationHandler {
    def apply[Real, Raw](fun: Real => Raw): RealInvocationHandler[Real, Raw] =
      new RealInvocationHandler[Real, Raw] {
        def toRaw(real: Real) = fun(real)
      }
  }

  @implicitNotFound("This RPC framework doesn't support RPC methods that return ${Real} " +
    "or some implicit dependencies may be missing (e.g. Reader[A] when result type is Future[A])")
  trait RawInvocationHandler[Real] {
    def toReal(rawRpc: RawRPC, rpcName: String, argLists: List[List[RawValue]]): Real
  }
  object RawInvocationHandler {
    def apply[Real](fun: (RawRPC, String, List[List[RawValue]]) => Real): RawInvocationHandler[Real] =
      new RawInvocationHandler[Real] {
        def toReal(rawRpc: RawRPC, rpcName: String, argLists: List[List[RawValue]]) = fun(rawRpc, rpcName, argLists)
      }
  }

  trait RawRPCUtils {
    protected def fail(rpcTpe: String, rawMethodName: String, methodName: String, args: List[List[RawValue]]) = {
      val argsRepr = args.map(_.mkString("[", ",", "]")).mkString("[", ",", "]")
      throw new Exception(s"$methodName in $rpcTpe with arguments $argsRepr cannot be handled by raw method $rawMethodName")
    }
  }

  trait AsRawRPC[T] {
    def asRaw(rpcImpl: T): RawRPC
  }

  object AsRawRPC {
    def apply[T](implicit asRawRPC: AsRawRPC[T]): AsRawRPC[T] = asRawRPC
  }

  /**
    * Materializes a factory of implementations of [[RawRPC]] which translate invocations of its raw methods
    * to invocations of actual methods on `rpcImpl`. Method arguments and results are serialized and deserialized
    * from/to [[RawValue]] using [[Reader]] and [[Writer]] typeclasses.
    */
  def materializeAsRaw[T]: AsRawRPC[T] = macro macros.rpc.RPCFrameworkMacros.asRawImpl[T]
  implicit def implicitlyMaterializeAsRaw[T]: AsRawRPC[T] = macro macros.rpc.RPCFrameworkMacros.asRawImpl[T]

  trait AsRealRPC[T] {
    def asReal(rawRpc: RawRPC): T
  }

  object AsRealRPC {
    @inline def apply[T](implicit asRealRPC: AsRealRPC[T]): AsRealRPC[T] = asRealRPC
  }

  /**
    * Materializes a factory of implementations of `T` which are proxies that implement all abstract methods of `T`
    * by forwarding them to `rawRpc`. Method arguments and results are serialized and deserialized
    * from/to [[RawValue]] using [[Reader]] and [[Writer]] typeclasses.
    */
  def materializeAsReal[T]: AsRealRPC[T] = macro macros.rpc.RPCFrameworkMacros.asRealImpl[T]
  implicit def implicitlyMaterializeAsReal[T]: AsRealRPC[T] = macro macros.rpc.RPCFrameworkMacros.asRealImpl[T]

  /** INTERNAL API */
  def tryToRaw[Real, Raw](real: Real): Raw = macro macros.rpc.RPCFrameworkMacros.tryToRaw[Real, Raw]

  trait RPCMetadata[T] {
    def name: String
    def annotations: List[MetadataAnnotation]
    def signatures: Map[String, Signature]
    def getterResults: Map[String, RPCMetadata[_]]
  }

  case class Signature(
    methodName: String,
    paramMetadata: List[List[ParamMetadata]],
    resultTypeMetadata: ResultTypeMetadata[_],
    annotations: List[MetadataAnnotation]
  )

  case class ParamMetadata(name: String, annotations: List[MetadataAnnotation], typeMetadata: ParamTypeMetadata[_])

  def materializeMetadata[T]: RPCMetadata[T] = macro macros.rpc.RPCFrameworkMacros.metadataImpl[T]
  implicit def implicitlyMaterializeMetadata[T]: RPCMetadata[T] = macro macros.rpc.RPCFrameworkMacros.metadataImpl[T]

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
