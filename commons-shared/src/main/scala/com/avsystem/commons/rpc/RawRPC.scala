package com.avsystem.commons
package rpc

import upickle.Js

import scala.concurrent.Future

/**
  * Author: ghik
  * Created: 27/05/15.
  */
trait RawRPC {
  protected def fail(rpcTpe: String, memberType: String, methodName: String, args: List[List[Js.Value]]) = {
    val argsRepr = args.map(_.mkString("[", ",", "]")).mkString("[", ",", "]")
    throw new Exception(s"Cannot find $memberType $methodName in $rpcTpe which could be invoked with arguments $argsRepr")
  }

  def fire(rpcName: String, argLists: List[List[Js.Value]]): Unit

  def call(rpcName: String, argLists: List[List[Js.Value]]): Future[Js.Value]

  def get(rpcName: String, argLists: List[List[Js.Value]]): RawRPC

  def resolveGetterChain(getterChain: List[RawInvocation]): RawRPC =
    getterChain.foldRight(this) {
      case (RawInvocation(rpcName, argLists), rawRpc) => rawRpc.get(rpcName, argLists)
    }
}

trait AsRawRPC[T] {
  def asRaw(rpcImpl: T): RawRPC
}

object AsRawRPC {
  /**
    * Materializes a factory of implementations of [[RawRPC]] which translate invocations of its `call` and `fire` methods
    * to invocations of actual methods on `rpcImpl`. Method arguments and results are serialized and deserialized
    * from/to JSON using `uPickle` library.
    *
    * Only calls to non-generic, abstract methods returning `Unit` or `Future` are supported.
    */
  implicit def materialize[T]: AsRawRPC[T] = macro com.avsystem.commons.macros.rpc.RPCMacros.asRawImpl[T]

  def apply[T](implicit asRawRPC: AsRawRPC[T]): AsRawRPC[T] = asRawRPC
}

trait AsRealRPC[T] {
  def asReal(rawRpc: RawRPC): T
}

object AsRealRPC {
  /**
    * Materializes a factory of implementations of `T` which are proxies that implement all abstract methods of `T`
    * by forwarding them to `rawRpc`. Method arguments and results are serialized and deserialized
    * from/to JSON using `uPickle` library.
    *
    * All abstract methods of `T` must be non-generic and return `Unit` or `Future`.
    */
  implicit def materialize[T]: AsRealRPC[T] = macro com.avsystem.commons.macros.rpc.RPCMacros.asRealImpl[T]

  def apply[T](implicit asRealRPC: AsRealRPC[T]): AsRealRPC[T] = asRealRPC
}

