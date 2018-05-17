package com.avsystem.commons
package rpc

import com.avsystem.commons.annotation.AnnotationAggregate
import com.avsystem.commons.serialization.GenCodec
import com.github.ghik.silencer.silent

trait DummyParamTag extends RpcTag with AnnotationAggregate

class header(name: String) extends DummyParamTag {
  @rpcName(name)
  type Implied
}

class renamed(int: Int, name: String) extends DummyParamTag {
  @rpcName(name)
  type Implied
}

sealed trait untagged extends DummyParamTag

sealed trait RestMethod extends RpcTag
class POST extends RestMethod
class GET extends RestMethod
class PUT extends RestMethod

@methodTag[RestMethod, RestMethod]
@paramTag[DummyParamTag, untagged]
trait NewRawRPC {
  @verbatim def fire(name: String, @optional ajdi: Opt[Int],
    @repeated args: Map[String, String]): Unit

  def call(name: String,
    @tagged[renamed] @repeated renamedArgs: => Map[String, String],
    @repeated args: Map[String, String]): Future[String]

  def get(name: String,
    @repeated args: List[String]): NewRawRPC

  @tagged[POST]
  def post(name: String,
    @tagged[header] @repeated @verbatim @auxiliary headers: Vector[String],
    @repeated body: MLinkedHashMap[String, String]): String
}
object NewRawRPC extends RawRPCCompanion[NewRawRPC] {
  override val implicits: this.type = this

  implicit def asRealRawFromGenCodec[T: GenCodec]: AsRealRaw[T, String] = ???
  implicit def futureAsRealRawFromGenCodec[T: GenCodec]: AsRealRaw[Future[T], Future[String]] = ???
}

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
  implicit val asRealRaw: AsRealRaw[NamedVarargs, NewRawRPC] = NewRawRPC.materializeAsRealRaw[NamedVarargs]
}

@silent
object NewRPCTest {
  implicit val innerRpcAsRealRaw: AsRealRaw[InnerRPC, NewRawRPC] = NewRawRPC.materializeAsRealRaw[InnerRPC]
  implicit val testRpcAsRealRaw: AsRealRaw[TestRPC, NewRawRPC] = NewRawRPC.materializeAsRealRaw[TestRPC]
}
