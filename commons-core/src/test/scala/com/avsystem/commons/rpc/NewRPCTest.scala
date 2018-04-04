package com.avsystem.commons
package rpc

import com.avsystem.commons.serialization.GenCodec
import com.github.ghik.silencer.silent

sealed trait RawValue
object RawValue {
  implicit def asRawFromGc[T: GenCodec]: AsRaw[T, RawValue] = ???
  implicit def asRealFromGc[T: GenCodec]: AsReal[T, RawValue] = ???
  implicit def futureAsRealFromGc[T: GenCodec]: AsReal[Future[T], Future[RawValue]] = ???
}

trait NewRawRPC {
  def getter(name: String, args: Map[String, RawValue]): NewRawRPC
  def invoke(name: String, args: Map[String, RawValue]): RawValue
  def invokeAsync(name: String, args: Map[String, RawValue]): Future[RawValue]
}

@silent
object NewRPCTest {
  implicit lazy val innerRpcAsReal: AsReal[InnerRPC, NewRawRPC] = AsReal.forRpc[InnerRPC, NewRawRPC]
  implicit lazy val testRpcAsReal: AsReal[TestRPC, NewRawRPC] = AsReal.forRpc[TestRPC, NewRawRPC]
}
