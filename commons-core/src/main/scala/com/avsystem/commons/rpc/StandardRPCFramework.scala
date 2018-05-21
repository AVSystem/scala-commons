package com.avsystem.commons
package rpc

/**
  * Mix in this trait into your RPC framework to support remote procedures, i.e. fire-and-forget methods
  * with `Unit` return type.
  */
trait ProcedureRPCFramework extends RPCFramework {
  type RawRPC <: ProcedureRawRPC

  trait ProcedureRawRPC { this: RawRPC =>
    @verbatim def fire(rpcName: String, @multi args: List[RawValue]): Unit
  }
}

/**
  * Mix in this trait into your RPC framework to support remote functions, i.e. methods which asynchronously
  * return some result (`Future[A]` where `A` has a `Reader` and `Writer`).
  */
trait FunctionRPCFramework extends RPCFramework {
  type RawRPC <: FunctionRawRPC

  trait FunctionRawRPC { this: RawRPC =>
    def call(rpcName: String, @multi args: List[RawValue]): Future[RawValue]
  }

  implicit def readerBasedFutureAsReal[T: Reader]: AsReal[Future[RawValue], Future[T]] =
    AsReal.create(_.mapNow(read[T]))
  implicit def writerBasedFutureAsRaw[T: Writer]: AsRaw[Future[RawValue], Future[T]] =
    AsRaw.create(_.mapNow(write[T]))
}

/**
  * Mix in this trait into your RPC framework to support getters, i.e. methods that return RPC subinterfaces
  */
trait GetterRPCFramework extends RPCFramework {
  type RawRPC <: GetterRawRPC

  case class RawInvocation(rpcName: String, args: List[RawValue])

  trait GetterRawRPC { this: RawRPC =>
    def get(rpcName: String, @multi args: List[RawValue]): RawRPC

    def resolveGetterChain(getters: List[RawInvocation]): RawRPC =
      getters.foldRight(this)((inv, rpc) => rpc.get(inv.rpcName, inv.args))
  }
}

trait StandardRPCFramework extends GetterRPCFramework with FunctionRPCFramework with ProcedureRPCFramework {
  trait RawRPC extends GetterRawRPC with FunctionRawRPC with ProcedureRawRPC
  object RawRPC extends BaseRawRpcCompanion

  trait FullRPCInfo[T] extends BaseFullRPCInfo[T]
}

trait OneWayRPCFramework extends GetterRPCFramework with ProcedureRPCFramework {
  trait RawRPC extends GetterRawRPC with ProcedureRawRPC
  object RawRPC extends BaseRawRpcCompanion

  trait FullRPCInfo[T] extends BaseFullRPCInfo[T]
}
