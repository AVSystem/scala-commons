package com.avsystem.commons
package rpc

import scala.annotation.implicitNotFound
import scala.language.higherKinds

/**
  * Author: ghik
  * Created: 27/05/15.
  */

trait RPCFramework {
  type RawValue
  type Reader[T]
  type Writer[T]
  type RawRPC

  def read[T: Reader](raw: RawValue): T
  def write[T: Writer](value: T): RawValue

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
  def materializeAsRaw[T]: AsRawRPC[T] = macro macros.rpc.RPCMacros.asRawImpl[T]
  implicit def implicitlyMaterializeAsRaw[T]: AsRawRPC[T] = macro macros.rpc.RPCMacros.asRawImpl[T]

  trait AsRealRPC[T] {
    def asReal(rawRpc: RawRPC): T
  }

  object AsRealRPC {
    def apply[T](implicit asRealRPC: AsRealRPC[T]): AsRealRPC[T] = asRealRPC
  }

  /**
    * Materializes a factory of implementations of `T` which are proxies that implement all abstract methods of `T`
    * by forwarding them to `rawRpc`. Method arguments and results are serialized and deserialized
    * from/to [[RawValue]] using [[Reader]] and [[Writer]] typeclasses.
    */
  def materializeAsReal[T]: AsRealRPC[T] = macro macros.rpc.RPCMacros.asRealImpl[T]
  implicit def implicitlyMaterializeAsReal[T]: AsRealRPC[T] = macro macros.rpc.RPCMacros.asRealImpl[T]

  /** INTERNAL API */
  def tryToRaw[Real, Raw](real: Real, onFailure: Nothing): Raw = macro macros.rpc.RPCMacros.tryToRaw[Real, Raw]
}
