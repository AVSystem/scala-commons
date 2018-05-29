package com.avsystem.commons
package rpc

object DummyRPC extends StandardRPCFramework {
  type RawValue = Any

  type Reader[T] = DummyImplicit
  type Writer[T] = DummyImplicit
  type ParamTypeMetadata[T] = TypeName[T]
  type ResultTypeMetadata[T] = ClassTag[T]

  def read[T: Reader](raw: Any): T = raw.asInstanceOf[T]
  def write[T: Writer](value: T): Any = value
}

case class TypeName[T](name: String)
object TypeName {
  def get[T](implicit tn: TypeName[T]): String = tn.name

  implicit def typeName[T: ClassTag]: TypeName[T] =
    TypeName(classTag[T].runtimeClass.getSimpleName)
}
