package com.avsystem.commons
package rpc

import com.avsystem.commons.serialization.{GenCodec, Input, Output}

/**
  * Mix in this trait into your RPC framework to use [[GenCodec]] for serialization.
  */
trait GenCodecSerializationFramework extends RPCFramework {
  type Writer[T] = GenCodec[T]
  type Reader[T] = GenCodec[T]

  /** Converts value of type `T` into `RawValue`. */
  final def write[T: Writer](value: T): RawValue = {
    var result: RawValue = null.asInstanceOf[RawValue]
    GenCodec.write[T](outputSerialization(result = _), value)
    result
  }

  /** Converts `RawValue` into value of type `T`. */
  final def read[T: Reader](raw: RawValue): T =
    GenCodec.read[T](inputSerialization(raw))

  /** Creates an `Input` for data marshalling. */
  def inputSerialization(value: RawValue): Input

  /** Creates an `Output` for data unmarshalling. */
  def outputSerialization(valueConsumer: RawValue => Unit): Output
}

/**
  * Mix in this trait into your RPC framework to support remote procedures, i.e. fire-and-forget methods
  * with `Unit` return type.
  */
trait ProcedureRPCFramework extends RPCFramework {
  type RawRPC <: ProcedureRawRPC

  trait ProcedureRawRPC { this: RawRPC =>
    def fire(rpcName: String, argLists: List[List[RawValue]]): Unit
  }

  implicit val ProcedureRealHandler: RealInvocationHandler[Unit, Unit] =
    RealInvocationHandler[Unit, Unit](_ => ())
  implicit val ProcedureRawHandler: RawInvocationHandler[Unit] =
    RawInvocationHandler[Unit](_.fire(_, _))
}

/**
  * Mix in this trait into your RPC framework to support remote functions, i.e. methods which asynchronously
  * return some result (`Future[A]` where `A` has a `Reader` and `Writer`).
  */
trait FunctionRPCFramework extends RPCFramework {
  type RawRPC <: FunctionRawRPC

  trait FunctionRawRPC { this: RawRPC =>
    def call(rpcName: String, argLists: List[List[RawValue]]): Future[RawValue]
  }

  implicit def FunctionRealHandler[A: Writer]: RealInvocationHandler[Future[A], Future[RawValue]] =
    RealInvocationHandler[Future[A], Future[RawValue]](_.mapNow(write[A]))
  implicit def FunctionRawHandler[A: Reader]: RawInvocationHandler[Future[A]] =
    RawInvocationHandler[Future[A]]((rawRpc, rpcName, argLists) => rawRpc.call(rpcName, argLists).mapNow(read[A]))
}

/**
  * Mix in this trait into your RPC framework to support getters, i.e. methods that return RPC subinterfaces
  */
trait GetterRPCFramework extends RPCFramework {
  type RawRPC <: GetterRawRPC

  case class RawInvocation(rpcName: String, argLists: List[List[RawValue]])

  trait GetterRawRPC { this: RawRPC =>
    def get(rpcName: String, argLists: List[List[RawValue]]): RawRPC

    def resolveGetterChain(getters: List[RawInvocation]): RawRPC =
      getters.foldRight(this)((inv, rpc) => rpc.get(inv.rpcName, inv.argLists))
  }

  // these must be macros in order to properly handle recursive RPC types
  implicit def getterRealHandler[T](implicit ev: IsRPC[T]): RealInvocationHandler[T, RawRPC] = macro macros.rpc.RPCFrameworkMacros.getterRealHandler[T]
  implicit def getterRawHandler[T](implicit ev: IsRPC[T]): RawInvocationHandler[T] = macro macros.rpc.RPCFrameworkMacros.getterRawHandler[T]

  final class GetterRealHandler[T: AsRawRPC] extends RealInvocationHandler[T, RawRPC] {
    def toRaw(real: T) = AsRawRPC[T].asRaw(real)
  }
  final class GetterRawHandler[T: AsRealRPC] extends RawInvocationHandler[T] {
    def toReal(rawRpc: RawRPC, rpcName: String, argLists: List[List[RawValue]]) = AsRealRPC[T].asReal(rawRpc.get(rpcName, argLists))
  }
}

trait StandardRPCFramework extends GetterRPCFramework with FunctionRPCFramework with ProcedureRPCFramework {
  trait RawRPC extends GetterRawRPC with FunctionRawRPC with ProcedureRawRPC
  trait FullRPCInfo[T] extends BaseFullRPCInfo[T]
}

trait OneWayRPCFramework extends GetterRPCFramework with ProcedureRPCFramework {
  trait RawRPC extends GetterRawRPC with ProcedureRawRPC
  trait FullRPCInfo[T] extends BaseFullRPCInfo[T]
}
