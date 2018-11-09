package com.avsystem.commons
package rpc

import com.avsystem.commons.serialization.GenCodec.ReadFailure

object DummyRPC extends StandardRPCFramework {
  type RawValue = Any

  type Reader[T] = ClassTag[T]
  type Writer[T] = DummyImplicit
  type ParamTypeMetadata[T] = TypeName[T]
  type ResultTypeMetadata[T] = ClassTag[T]

  def read[T: Reader](raw: Any): T = raw match {
    case t: T => t
    case _ => throw new ReadFailure(s"Expected instance of ${classTag[T].runtimeClass}, got $raw")
  }
  
  def write[T: Writer](value: T): Any = value
}

case class TypeName[T](name: String)
object TypeName {
  def get[T](implicit tn: TypeName[T]): String = tn.name

  implicit def typeName[T: ClassTag]: TypeName[T] =
    TypeName(classTag[T].runtimeClass.getSimpleName)
}
