package com.avsystem.commons
package rpc

import com.avsystem.commons.annotation.AnnotationAggregate
import com.avsystem.commons.serialization.GenCodec
import com.github.ghik.silencer.silent

class POST extends RPCAnnotation
class header(name: String) extends RPCAnnotation with AnnotationAggregate {
  @RPCName(name)
  type Implied
}

trait NewRawRPC {
  @verbatim def fire(name: String, @optional ajdi: Opt[Int],
    @namedRepeated args: Map[String, String]): Unit

  def call(name: String,
    @annotatedWith[RPCName] @namedRepeated renamedArgs: => Map[String, String],
    @namedRepeated args: Map[String, String]): Future[String]

  def get(name: String,
    @repeated args: List[String]): NewRawRPC

  @annotatedWith[POST]
  def post(name: String,
    @annotatedWith[header] @namedRepeated @verbatim headers: Map[String, String],
    @namedRepeated body: MLinkedHashMap[String, String]): String
}
object NewRawRPC extends RawRPCCompanion[NewRawRPC] {
  override val implicits: this.type = this

  implicit def asRealRawFromGenCodec[T: GenCodec]: AsRealRaw[T, String] = ???
  implicit def futureAsRealRawFromGenCodec[T: GenCodec]: AsRealRaw[Future[T], Future[String]] = ???
}

class EnhancedName(int: Int, name: String) extends AnnotationAggregate {
  @RPCName(name)
  type Implied
}
trait NamedVarargs {
  def varargsMethod(krap: String, dubl: Double)(czy: Boolean, @EnhancedName(42, "nejm") ints: Int*): Future[Unit]
  def defaultValueMethod(int: Int = 0, bul: Boolean): Future[Unit]
  def flames(arg: String, otherArg: => Int, varargsy: Double*): Unit
  def overload(int: Int): Unit
  def overload: NamedVarargs
  def getit(stuff: String, otherStuff: List[Int]): NamedVarargs
  @POST def postit(arg: String, @header("X-Bar") bar: String, int: Int, @header("X-Foo") foo: String): String
}
object NamedVarargs {
  implicit val asRealRaw: AsRealRaw[NamedVarargs, NewRawRPC] = NewRawRPC.materializeAsRealRaw[NamedVarargs].showAst
}

@silent
object NewRPCTest {
  implicit val innerRpcAsRealRaw: AsRealRaw[InnerRPC, NewRawRPC] = NewRawRPC.materializeAsRealRaw[InnerRPC]
  implicit val testRpcAsRealRaw: AsRealRaw[TestRPC, NewRawRPC] = NewRawRPC.materializeAsRealRaw[TestRPC]
}
