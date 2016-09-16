package com.avsystem.commons
package rpc

/**
  * Convenience class to be extended by companion objects of RPC traits. It automatically adds implicits for
  * `AsRawRPC`, `AsRealRPC` and `RPCMetadata` given that `RPCFramework` implementation is known.
  *
  * It's not required for RPC traits companion objects to extend this trait but it's recommended because it
  * reduces problems with incremental compilation and duplicated bytecode.
  */
abstract class RPCTypeClasses[F <: RPCFramework with Singleton, T](implicit fullRPCMetadata: AllRPCTypeClasses[F, T]) {
  implicit val asRawRPC: F#AsRawRPC[T] = fullRPCMetadata.asRawRPC
  implicit val asRealRPC: F#AsRealRPC[T] = fullRPCMetadata.asRealRPC
  implicit val rpcMetadata: RPCMetadata[T] = fullRPCMetadata.metadata
}

case class AllRPCTypeClasses[F <: RPCFramework with Singleton, T](asRealRPC: F#AsRealRPC[T], asRawRPC: F#AsRawRPC[T], metadata: RPCMetadata[T])
object AllRPCTypeClasses {
  implicit def materialize[F <: RPCFramework with Singleton, T]: AllRPCTypeClasses[F, T] = macro macros.rpc.RPCMacros.allRpcTypeClassesImpl[F, T]
}
