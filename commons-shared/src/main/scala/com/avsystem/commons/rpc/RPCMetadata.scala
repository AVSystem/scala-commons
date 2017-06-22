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

case class ParamMetadata(name: String, tpe: ParamMetadata.SimplifiedType, annotations: List[MetadataAnnotation])

object ParamMetadata {
  sealed trait SimplifiedType
  case object DoubleType extends SimplifiedType
  case object FloatType extends SimplifiedType
  case object LongType extends SimplifiedType
  case object IntType extends SimplifiedType
  case object CharType extends SimplifiedType
  case object ShortType extends SimplifiedType
  case object ByteType extends SimplifiedType
  case object StringType extends SimplifiedType
  case object ObjectType extends SimplifiedType
}