package com.avsystem.commons
package rpc.akka

import com.avsystem.commons.meta._
import com.avsystem.commons.rpc._
import monix.reactive.Observable

trait MonixRPCFramework extends RPCFramework {
  override type RawRPC <: MonixRawRPC

  trait MonixRawRPC { this: RawRPC =>
    @multi def observe(@composite invocation: RawInvocation): Observable[RawValue]
  }

  implicit def readerBasedObservableAsReal[T: Reader]: AsReal[Observable[RawValue], Observable[T]] =
    AsReal.create(_.map(read[T]))
  implicit def writerBasedObservableAsRaw[T: Writer]: AsRaw[Observable[RawValue], Observable[T]] =
    AsRaw.create(_.map(write[T]))
}
