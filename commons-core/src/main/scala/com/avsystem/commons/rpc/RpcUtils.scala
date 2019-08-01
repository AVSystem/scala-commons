package com.avsystem.commons
package rpc

import com.avsystem.commons.macros.misc.MiscMacros

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

class InvalidRpcCall(msg: String, cause: Throwable = null)
  extends RuntimeException(msg, cause)

class InvalidRpcArgument(val rpcName: String, val argName: String, cause: Throwable)
  extends InvalidRpcCall(s"Argument $argName of RPC $rpcName is invalid: ${cause.getMessage}", cause)

class MissingRpcArgument(val rpcName: String, val argName: String)
  extends InvalidRpcCall(s"Argument $argName of RPC $rpcName is missing")

class UnknownRpc(val rpcName: String, val rawMethodName: String)
  extends InvalidRpcCall(s"Unknown RPC $rpcName for raw method $rawMethodName")

class MissingOptionalRpc(val rawMethodName: String)
  extends InvalidRpcCall(s"No matching RPC for optional raw method $rawMethodName")

object RpcUtils {
  def createEmpty[Coll](cbf: CanBuildFrom[Nothing, Nothing, Coll]): Coll =
    createBuilder[Nothing, Coll](cbf, 0).result()

  def createBuilder[Elem, Coll](cbf: CanBuildFrom[Nothing, Elem, Coll], size: Int): mutable.Builder[Elem, Coll] = {
    val b = cbf()
    b.sizeHint(size)
    b
  }

  def readArg[Raw, Real](rpcName: String, argName: String, asReal: AsReal[Raw, Real], raw: Raw): Real =
    try asReal.asReal(raw) catch {
      case NonFatal(cause) => invalidArg(rpcName, argName, cause)
    }

  def invalidArg(rpcName: String, argName: String, cause: Throwable): Nothing =
    throw new InvalidRpcArgument(rpcName, argName, cause)

  def missingArg(rpcName: String, argName: String): Nothing =
    throw new MissingRpcArgument(rpcName, argName)

  def unknownRpc(rpcName: String, rawMethodName: String): Nothing =
    throw new UnknownRpc(rpcName, rawMethodName)

  def missingOptionalRpc(rawMethodName: String): Nothing =
    throw new MissingOptionalRpc(rawMethodName)

  def interceptEnc[NewRaw, Raw, Real](asRaw: AsRaw[NewRaw, Real], interceptor: EncodingInterceptor[NewRaw, Raw]): AsRaw[Raw, Real] =
    AsRaw.create(real => interceptor.toOriginalRaw(asRaw.asRaw(real)))

  def interceptDec[NewRaw, Raw, Real](asReal: AsReal[NewRaw, Real], interceptor: DecodingInterceptor[NewRaw, Raw]): AsReal[Raw, Real] =
    AsReal.create(raw => asReal.asReal(interceptor.toNewRaw(raw)))

  def compilationError(error: String): Nothing = macro MiscMacros.compilationError
}
