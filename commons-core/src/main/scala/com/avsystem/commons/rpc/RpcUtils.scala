package com.avsystem.commons
package rpc

/**
  * @author ghik
  */
object RpcUtils {
  def missingArg(rpcName: String, argName: String): Nothing =
    throw new IllegalArgumentException(s"Can't interpret raw RPC call $rpcName: argument $argName is missing")

  def unknownRpc(rpcName: String, rawMethodName: String): Nothing =
    throw new IllegalArgumentException(s"RPC $rpcName does not map to raw method $rawMethodName")
}
