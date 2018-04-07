package com.avsystem.commons
package rpc

/**
  * @author ghik
  */
object RpcUtils {
  private object AbsentMarker
  private final val absentFun = (_: String) => AbsentMarker

  def getArg[T, R](args: PartialFunction[String, R], name: String, conv: AsReal[T, R], default: => T): T =
    args.applyOrElse[String, Any](name, absentFun) match {
      case AbsentMarker => default
      case v => conv.asReal(v.asInstanceOf[R])
    }

  def tryGetArg[T, R](args: PartialFunction[String, R], name: String, conv: AsReal[T, R], rpcName: String): T =
    conv.asReal(args.applyOrElse(name, (_: String) => missingArg(rpcName, name)))

  def missingArg(rpcName: String, argName: String): Nothing =
    throw new IllegalArgumentException(s"Can't interpret raw RPC call $rpcName: argument $argName is missing")

  def unknownRpc(rpcName: String, rawMethodName: String): Nothing =
    throw new IllegalArgumentException(s"RPC $rpcName does not map to raw method $rawMethodName")
}
