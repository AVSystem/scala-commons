package com.avsystem.commons
package rpc

import com.avsystem.commons.annotation.AnnotationAggregate
import com.avsystem.commons.serialization.GenCodec

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
trait NewRawRpc {
  @verbatim def fire(name: String, @optional @auxiliary ajdi: Opt[Int],
    @repeated args: Map[String, String]): Unit

  def call(name: String,
    @tagged[renamed] @repeated renamedArgs: => Map[String, String],
    @repeated args: Map[String, String]): Future[String]

  def get(name: String,
    @repeated args: List[String]): NewRawRpc

  @tagged[POST]
  def post(name: String,
    @tagged[header] @repeated @verbatim headers: Vector[String],
    @repeated body: MLinkedHashMap[String, String]): String
}
object NewRawRpc extends RawRpcCompanion[NewRawRpc] {
  override val implicits: this.type = this

  implicit def asRealRawFromGenCodec[T: GenCodec]: AsRealRaw[String, T] = ???
  implicit def futureAsRealRawFromGenCodec[T: GenCodec]: AsRealRaw[Future[String], Future[T]] = ???
}
