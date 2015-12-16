package com.avsystem.commons
package rpc

import scala.concurrent.Future
import scala.language.higherKinds

/**
  * Author: ghik
  * Created: 27/05/15.
  */

trait RPCFramework {
  type RawValue
  type Reader[T]
  type Writer[T]

  def read[T: Reader](raw: RawValue): T
  def write[T: Writer](value: T): RawValue

  case class RawInvocation(rpcName: String, argLists: List[List[RawValue]])

  trait RawRPC {
    protected def fail(rpcTpe: String, memberType: String, methodName: String, args: List[List[RawValue]]) = {
      val argsRepr = args.map(_.mkString("[", ",", "]")).mkString("[", ",", "]")
      throw new Exception(s"Cannot find $memberType $methodName in $rpcTpe which could be invoked with arguments $argsRepr")
    }

    def fire(rpcName: String, argLists: List[List[RawValue]]): Unit

    def call(rpcName: String, argLists: List[List[RawValue]]): Future[RawValue]

    def get(rpcName: String, argLists: List[List[RawValue]]): RawRPC

    def resolveGetterChain(getterChain: List[RawInvocation]): RawRPC =
      getterChain.foldRight(this) {
        case (RawInvocation(rpcName, argLists), rawRpc) => rawRpc.get(rpcName, argLists)
      }
  }

  trait AsRawRPC[T] {
    def asRaw(rpcImpl: T): RawRPC
  }

  object AsRawRPC {
    def apply[T](implicit asRawRPC: AsRawRPC[T]): AsRawRPC[T] = asRawRPC
  }

  /**
    * Materializes a factory of implementations of [[RawRPC]] which translate invocations of its `call` and `fire` methods
    * to invocations of actual methods on `rpcImpl`. Method arguments and results are serialized and deserialized
    * from/to JSON using `uPickle` library.
    *
    * Only calls to non-generic, abstract methods returning `Unit` or `Future` are supported.
    */
  implicit def materializeAsRaw[T]: AsRawRPC[T] = macro macros.rpc.RPCMacros.asRawImpl[T]

  trait AsRealRPC[T] {
    def asReal(rawRpc: RawRPC): T
  }

  object AsRealRPC {
    def apply[T](implicit asRealRPC: AsRealRPC[T]): AsRealRPC[T] = asRealRPC
  }

  /**
    * Materializes a factory of implementations of `T` which are proxies that implement all abstract methods of `T`
    * by forwarding them to `rawRpc`. Method arguments and results are serialized and deserialized
    * from/to JSON using `uPickle` library.
    *
    * All abstract methods of `T` must be non-generic and return `Unit` or `Future`.
    */
  implicit def materializeAsReal[T]: AsRealRPC[T] = macro macros.rpc.RPCMacros.asRealImpl[T]
}
