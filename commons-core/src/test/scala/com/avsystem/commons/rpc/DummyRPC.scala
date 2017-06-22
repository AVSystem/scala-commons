package com.avsystem.commons
package rpc

object DummyRPC extends StandardRPCFramework {
  type RawValue = Any

  type Reader[T] = DummyImplicit
  type Writer[T] = DummyImplicit
  class ParamTypeMetadata[+T]
  implicit object ParamTypeMetadata extends ParamTypeMetadata[Nothing]

  def read[T: Reader](raw: Any): T = raw.asInstanceOf[T]
  def write[T: Writer](value: T): Any = value
}
