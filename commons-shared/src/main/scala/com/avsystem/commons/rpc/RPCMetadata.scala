package com.avsystem.commons
package rpc

/**
  * Author: ghik
  * Created: 25/02/16.
  */
object RPCMetadata {
  def apply[T](implicit metadata: RPCMetadata[T]): RPCMetadata[T] = metadata
  implicit def materialize[T]: RPCMetadata[T] = macro macros.rpc.RPCMacros.materializeMetadata[T]
}

trait RPCMetadata[T] {
  def name: String
  def annotations: List[MetadataAnnotation]
  def methodsByRpcName: Map[String, MethodMetadata]

  def getterResultMetadata(rpcName: String): RPCMetadata[_] =
    methodsByRpcName.get(rpcName).collect({ case GetterMetadata(_, resultMetadata) => resultMetadata }).get
}

case class Signature(
  methodName: String,
  paramMetadata: List[List[ParamMetadata]],
  annotations: List[MetadataAnnotation]
)

case class ParamMetadata(name: String, annotations: List[MetadataAnnotation])

sealed trait MethodMetadata {
  def signature: Signature
}
case class ProcedureMetadata(signature: Signature) extends MethodMetadata
case class FunctionMetadata(signature: Signature) extends MethodMetadata
case class GetterMetadata(signature: Signature, resultMetadata: RPCMetadata[_]) extends MethodMetadata
