package com.avsystem.commons
package rpc

/**
  * Author: ghik
  * Created: 16/12/15.
  */
object DummyRPC extends RPCFramework {
  type RawValue = Any

  type Reader[T] = DummyRW[T]
  type Writer[T] = DummyRW[T]

  def read[T: DummyRW](raw: Any): T = raw.asInstanceOf[T]
  def write[T: DummyRW](value: T): Any = value

  sealed trait DummyRW[T]
  object DummyRW {
    implicit def dummyRW[T]: DummyRW[T] = null
  }
}
