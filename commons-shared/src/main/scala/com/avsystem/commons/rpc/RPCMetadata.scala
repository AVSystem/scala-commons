package com.avsystem.commons
package rpc

object RPCMetadata {
  def apply[T](implicit metadata: RPCMetadata[T]): RPCMetadata[T] = metadata
  implicit def implicitlyMaterialize[T]: RPCMetadata[T] = macro macros.rpc.RPCMacros.materializeMetadata[T]
  def materialize[T]: RPCMetadata[T] = macro macros.rpc.RPCMacros.materializeMetadata[T]
}

trait RPCMetadata[T] {
  def name: String
  def annotations: List[MetadataAnnotation]
  def signatures: Map[String, Signature]
  def getterResults: Map[String, RPCMetadata[_]]
}

case class Signature(
  methodName: String,
  paramMetadata: List[List[ParamMetadata]],
  annotations: List[MetadataAnnotation]
)

case class ParamMetadata(name: String, annotations: List[MetadataAnnotation])
