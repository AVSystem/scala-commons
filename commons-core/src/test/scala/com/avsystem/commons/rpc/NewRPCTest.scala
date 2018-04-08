package com.avsystem.commons
package rpc

import com.avsystem.commons.annotation.AnnotationAggregate
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
  def invoke(name: String,
    @annotatedWith[RPCName] renamedArgs: Map[String,RawValue],
    args: Map[String, RawValue]): RawValue
  def invokeAsync(name: String, args: Map[String, RawValue]): Future[RawValue]
}

class EnhancedName(int: Int, name: String) extends AnnotationAggregate {
  @RPCName(name)
  type Implied
}
trait NamedVarargs {
  def varargsMethod(krap: String, dubl: Double)(czy: Boolean, @EnhancedName(42, "nejm") ints: Int*): Unit
  def defaultValueMethod(int: Int = 0, bul: Boolean): Unit
  def overload(int: Int): Unit
  def overload: NamedVarargs
}
object NamedVarargs {
  implicit val asReal: AsReal[NamedVarargs, NewRawRPC] = AsReal.forRpc[NamedVarargs, NewRawRPC].showAst
  implicit val asRaw: AsRaw[NamedVarargs, NewRawRPC] = AsRaw.forRpc[NamedVarargs, NewRawRPC].showAst
}

@silent
object NewRPCTest {
  implicit val innerRpcAsReal: AsReal[InnerRPC, NewRawRPC] = AsReal.forRpc[InnerRPC, NewRawRPC]
  implicit val testRpcAsReal: AsReal[TestRPC, NewRawRPC] = AsReal.forRpc[TestRPC, NewRawRPC]
  implicit val innerRpcAsRaw: AsRaw[InnerRPC, NewRawRPC] = AsRaw.forRpc[InnerRPC, NewRawRPC]
  implicit val testRpcAsRaw: AsRaw[TestRPC, NewRawRPC] = AsRaw.forRpc[TestRPC, NewRawRPC]
}
