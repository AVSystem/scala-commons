package com.avsystem.commons.rpc

trait AsRawMacros { this: AsRaw.type =>
  inline def materialize[Raw, Real]: AsRaw[Raw, Real] = ???
  inline def materializeForApi[Raw, Real]: AsRaw[Raw, Real] = ???
}

trait AsRealMacros { this: AsReal.type =>
  inline def materialize[Raw, Real]: AsReal[Raw, Real] = ???
}

trait AsRawRealMacros { this: AsRawReal.type =>
  inline def materialize[Raw, Real]: AsRawReal[Raw, Real] = ???
}

trait RpcMetadataMacros { this: RpcMetadata.type =>
  inline def materialize[M[_], Real]: M[Real] = ???
  inline def materializeForApi[M[_], Real]: M[Real] = ???
  inline def auto[T]: T = ???
}

trait RpcUtilsMacros {
  def compilationError(error: String): Nothing = ???
}

trait RawRpcMacros[Raw] {
  inline def materializeAsRaw[Real]: AsRaw[Raw, Real] = ???
  inline def materializeAsReal[Real]: AsReal[Raw, Real] = ???
  inline def materializeAsRawReal[Real]: AsRawReal[Raw, Real] = ???
  inline def materializeApiAsRaw[Real]: AsRaw[Raw, Real] = ???
}

trait RPCFrameworkMacros {
  inline def materializeAsRaw[T]: AsRaw[?, T] = ???
  inline def materializeAsReal[T]: AsReal[?, T] = ???
  inline def materializeAsRawReal[T]: AsRawReal[?, T] = ???
  inline def materializeMetadata[RealRPC]: Any = ???
  inline def materializeFullInfo[T]: Any = ???
}

trait FullRPCInfoMacros {
  inline implicit def asRealRPC: Any = ???
  inline implicit def asRawRPC: Any = ???
  inline implicit def metadata: Any = ???
}

trait RpcMetadataCompanionMacros[M[_]] {
  inline def materialize[Real]: M[Real] = ???
}

trait ApiMetadataCompanionMacros[M[_]] {
  inline def materialize[Real]: M[Real] = ???
}
