package com.avsystem.commons
package rpc

import com.avsystem.commons.macros.misc.MiscMacros

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

class RpcException(msg: String, cause: Throwable = null)
  extends RuntimeException(msg, cause)

object RpcUtils {
  def createEmpty[Coll](cbf: CanBuildFrom[Nothing, Nothing, Coll]): Coll =
    createBuilder[Nothing, Coll](cbf, 0).result()

  def createBuilder[Elem, Coll](cbf: CanBuildFrom[Nothing, Elem, Coll], size: Int): mutable.Builder[Elem, Coll] = {
    val b = cbf()
    b.sizeHint(size)
    b
  }

  def missingArg(rpcName: String, argName: String): Nothing =
    throw new RpcException(s"Can't interpret raw RPC call $rpcName: argument $argName is missing")

  def unknownRpc(rpcName: String, rawMethodName: String): Nothing =
    throw new RpcException(s"RPC $rpcName does not map to raw method $rawMethodName")

  def missingOptionalRpc(rawMethodName: String): Nothing =
    throw new RpcException(s"no matching real method for optional raw method $rawMethodName")

  def compilationError(error: String): Nothing = macro MiscMacros.compilationError
}
