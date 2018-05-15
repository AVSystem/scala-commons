package com.avsystem.commons
package rpc.akka

import com.avsystem.commons.rpc._
import monix.reactive.Observable

trait MonixRPCFramework extends RPCFramework {
  override type RawRPC <: MonixRawRPC

  trait MonixRawRPC { this: RawRPC =>
    def observe(rpcName: String, @repeated args: List[RawValue]): Observable[RawValue]
  }

  implicit def readerBasedObservableAsReal[T: Reader]: AsReal[Observable[T], Observable[RawValue]] =
    new AsReal[Observable[T], Observable[RawValue]] {
      def asReal(raw: Observable[RawValue]): Observable[T] = raw.map(read[T])
    }
  implicit def writerBasedObservableAsRaw[T: Writer]: AsRaw[Observable[T], Observable[RawValue]] =
    new AsRaw[Observable[T], Observable[RawValue]] {
      def asRaw(raw: Observable[T]): Observable[RawValue] = raw.map(write[T])
    }
}
