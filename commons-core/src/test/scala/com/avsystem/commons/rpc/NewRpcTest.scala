package com.avsystem.commons
package rpc

import com.github.ghik.silencer.silent

trait SomeBase {
  @POST def postit(arg: String, @header("X-Bar") bar: String, int: Int, @header("X-Foo") foo: String): String
}

trait NamedVarargs extends SomeBase {
  def varargsMethod(krap: String, dubl: Double)(czy: Boolean, @renamed(42, "nejm") ints: Int*): Future[Unit]
  def defaultValueMethod(int: Int = 0, bul: Boolean): Future[Unit]
  def flames(arg: String, otherArg: => Int, varargsy: Double*): Unit
  def overload(int: Int): Unit
  def overload: NamedVarargs
  def getit(stuff: String, otherStuff: List[Int]): NamedVarargs
  def postit(arg: String, bar: String, int: Int, foo: String): String
}
object NamedVarargs {
  implicit val asRealRaw: AsRealRaw[NamedVarargs, NewRawRpc] = NewRawRpc.materializeAsRealRaw[NamedVarargs].showAst
}

object NewRpcTest {
  implicit val innerRpcAsRealRaw: AsRealRaw[InnerRPC, NewRawRpc] = NewRawRpc.materializeAsRealRaw[InnerRPC]
  @silent
  implicit val testRpcAsRealRaw: AsRealRaw[TestRPC, NewRawRpc] = NewRawRpc.materializeAsRealRaw[TestRPC]
}
