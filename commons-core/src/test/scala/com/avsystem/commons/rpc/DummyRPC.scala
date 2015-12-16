package com.avsystem.commons
package rpc

/**
  * Author: ghik
  * Created: 16/12/15.
  */
object DummyRPC extends RPCFramework {
  type RawValue = Any

  type Reader[T] = DummyImplicit
  type Writer[T] = DummyImplicit

  def read[T: Reader](raw: Any): T = raw.asInstanceOf[T]
  def write[T: Writer](value: T): Any = value
}
