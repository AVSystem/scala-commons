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
  @verbatim def fire(name: String)(@optional @auxiliary ajdi: Opt[Int],
    @multi args: Map[String, String]): Unit

  def call(name: String)(
    @tagged[renamed] @multi renamedArgs: => Map[String, String],
    @multi args: Map[String, String]): Future[String]

  def get(name: String)(
    @multi args: List[String]): NewRawRpc

  @tagged[POST]
  def post(name: String)(
    @tagged[header] @multi @verbatim headers: Vector[String],
    @multi body: MLinkedHashMap[String, String]): String
}
object NewRawRpc extends RawRpcCompanion[NewRawRpc] {
  override val implicits: this.type = this

  implicit def asRealRawFromGenCodec[T: GenCodec]: AsRealRaw[String, T] = ???
  implicit def futureAsRealRawFromGenCodec[T: GenCodec]: AsRealRaw[Future[String], Future[T]] = ???
}
