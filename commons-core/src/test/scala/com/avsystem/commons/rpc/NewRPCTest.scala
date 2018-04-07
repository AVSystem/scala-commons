package com.avsystem.commons
package rpc

import com.avsystem.commons.serialization.GenCodec
import com.github.ghik.silencer.silent

sealed trait RawValue
object RawValue {
  implicit def asRawFromGc[T: GenCodec]: AsRaw[T, RawValue] = ???
  implicit def asRealFromGc[T: GenCodec]: AsReal[T, RawValue] = ???
  implicit def futureAsRealFromGc[T: GenCodec]: AsReal[Future[T], Future[RawValue]] = ???
  implicit def futureAsRawFromGc[T: GenCodec]: AsRaw[Future[T], Future[RawValue]] = ???
}

trait NewRawRPC {
  def getter(name: String, args: Map[String, RawValue]): NewRawRPC
  def invoke(name: String, args: Map[String, RawValue]): RawValue
  def invokeAsync(name: String, args: Map[String, RawValue]): Future[RawValue]
}

trait NamedVarargs {
  def namedArgMethod(int: => Int): Unit
  def varargsMethod(ints: Int*): Unit
  def defaultValueMethod(int: Int = 0): Unit
}
object NamedVarargs {
  implicit val asReal: AsReal[NamedVarargs, NewRawRPC] = AsReal.forRpc[NamedVarargs, NewRawRPC]
  implicit val asRaw: AsRaw[NamedVarargs, NewRawRPC] = AsRaw.forRpc[NamedVarargs, NewRawRPC].showAst
}

@silent
object NewRPCTest {
  implicit val innerRpcAsReal: AsReal[InnerRPC, NewRawRPC] = AsReal.forRpc[InnerRPC, NewRawRPC]
  implicit val testRpcAsReal: AsReal[TestRPC, NewRawRPC] = AsReal.forRpc[TestRPC, NewRawRPC]
  implicit val innerRpcAsRaw: AsRaw[InnerRPC, NewRawRPC] = AsRaw.forRpc[InnerRPC, NewRawRPC]
  implicit val testRpcAsRaw: AsRaw[TestRPC, NewRawRPC] = AsRaw.forRpc[TestRPC, NewRawRPC]
}
