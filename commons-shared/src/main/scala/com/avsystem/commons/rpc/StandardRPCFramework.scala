package com.avsystem.commons
package rpc

import com.avsystem.commons.concurrent.RunNowEC

import scala.concurrent.Future

trait ProcedureRPCFramework extends RPCFramework {
  type RawRPC <: ProcedureRawRPC

  trait ProcedureRawRPC {
    def fire(rpcName: String, argLists: List[List[RawValue]]): Unit
  }

  implicit val UnitToRaw: RealInvocationHandler[Unit, Unit] =
    RealInvocationHandler[Unit, Unit](_ => ())
  implicit val FireHandler: RawInvocationHandler[Unit] =
    RawInvocationHandler[Unit](_.fire(_, _))
}

trait FunctionRPCFramework extends RPCFramework {
  type RawRPC <: FunctionRawRPC

  trait FunctionRawRPC {
    def call(rpcName: String, argLists: List[List[RawValue]]): Future[RawValue]
  }

  implicit def FutureToRaw[A: Writer]: RealInvocationHandler[Future[A], Future[RawValue]] =
    RealInvocationHandler[Future[A], Future[RawValue]](_.map(write[A] _)(RunNowEC))
  implicit def CallHandler[A: Reader]: RawInvocationHandler[Future[A]] =
    RawInvocationHandler[Future[A]]((rawRpc, rpcName, argLists) => rawRpc.call(rpcName, argLists).map(read[A] _)(RunNowEC))
}

trait GetterRPCFramework extends RPCFramework {
  type RawRPC <: GetterRawRPC

  trait GetterRawRPC {
    def get(rpcName: String, argLists: List[List[RawValue]]): RawRPC
  }

  // these must be macros in order to properly handle recursive RPC types
  implicit def RPCToRaw[T](implicit ev: IsRPC[T]): RealInvocationHandler[T, RawRPC] = macro macros.rpc.RPCMacros.RPCToRaw[T]
  implicit def GetHandler[T](implicit ev: IsRPC[T]): RawInvocationHandler[T] = macro macros.rpc.RPCMacros.GetHandler[T]
}

trait StandardRPCFramework extends GetterRPCFramework with FunctionRPCFramework with ProcedureRPCFramework {
  trait RawRPC extends GetterRawRPC with FunctionRawRPC with ProcedureRawRPC
}

trait OneWayRPCFramework extends GetterRPCFramework with ProcedureRPCFramework {
  trait RawRPC extends GetterRawRPC with ProcedureRawRPC
}
