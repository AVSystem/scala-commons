package com.avsystem.commons
package rpc

import com.avsystem.commons.annotation.AnnotationAggregate
import com.avsystem.commons.serialization.GenCodec

trait DummyParamTag extends RpcTag with AnnotationAggregate

case class header(name: String) extends DummyParamTag {
  @rpcName(name)
  type Implied
}

case class renamed(int: Int, name: String) extends DummyParamTag {
  @rpcName(name)
  type Implied
}

sealed trait untagged extends DummyParamTag

sealed trait RestMethod extends RpcTag
case class POST() extends RestMethod
case class GET() extends RestMethod
case class PUT() extends RestMethod

@methodTag[RestMethod, RestMethod]
@paramTag[DummyParamTag, untagged]
trait NewRawRpc {
  @verbatim def fire(name: String)(
    @optional @auxiliary ajdi: Opt[Int],
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

@methodTag[RestMethod, RestMethod]
@paramTag[DummyParamTag, untagged]
case class NewRpcMetadata[T: TypeName](
  @verbatim procedures: Map[String, FireMetadata],
  functions: Map[String, CallMetadata[_]],
  getters: Map[String, GetterMetadata[_]],
  @tagged[POST] posters: Map[String, PostMetadata[_]]
) {
  def repr(open: List[NewRpcMetadata[_]]): String =
    if (open.contains(this)) "<recursive>" else {
      val membersStr =
        procedures.iterator.map({ case (n, v) => s"$n -> ${v.repr}" }).mkString("PROCEDURES:\n", "\n", "") + "\n" +
          functions.iterator.map({ case (n, v) => s"$n -> ${v.repr}" }).mkString("FUNCTIONS:\n", "\n", "") + "\n" +
          posters.iterator.map({ case (n, v) => s"$n -> ${v.repr}" }).mkString("POSTERS:\n", "\n", "") + "\n" +
          getters.iterator.map({ case (n, v) => s"$n -> ${v.repr(this :: open)}" }).mkString("GETTERS:\n", "\n", "")
      s"${TypeName.get[T]}\n" + membersStr.indent("  ")
    }

  override def toString: String = repr(Nil)
}
object NewRpcMetadata extends RpcMetadataCompanion[NewRpcMetadata]

trait MethodMetadata[T] {
  @reifyName def name: String
  @reifyName(rpcName = true) def rpcName: String
  def typeName: TypeName[T]

  def basicRepr: String = {
    val rpcNameStr = if (rpcName != name) s"<$rpcName>" else ""
    s"def $name$rpcNameStr: ${typeName.name}"
  }
}

case class FireMetadata(
  name: String, rpcName: String,
  @optional @auxiliary ajdi: Opt[ParameterMetadata[Int]],
  @multi args: Map[String, ParameterMetadata[_]]
) extends TypedMetadata[Unit] with MethodMetadata[Unit] {
  def typeName: TypeName[Unit] = TypeName("void")
  def repr: String =
    s"$basicRepr\n" +
      ajdi.fold("NO AJDI")(pm => s"AJDI: ${pm.repr}").indent("  ") + "\n" +
      args.iterator.map({ case (n, pm) => s"$n -> ${pm.repr}" }).mkString("ARGS:\n", "\n", "").indent("  ")
}

case class CallMetadata[T](
  name: String, rpcName: String,
  @tagged[renamed] @multi renamed: Map[String, ParameterMetadata[_]],
  @multi args: Map[String, ParameterMetadata[_]]
)(implicit val typeName: TypeName[T])
  extends TypedMetadata[Future[T]] with MethodMetadata[T] {
  def repr: String =
    s"$basicRepr\n" +
      renamed.iterator.map({ case (n, pm) => s"$n -> ${pm.repr}" }).mkString("RENAMED:\n", "\n", "").indent("  ") + "\n" +
      args.iterator.map({ case (n, pm) => s"$n -> ${pm.repr}" }).mkString("ARGS:\n", "\n", "").indent("  ")
}

case class GetterMetadata[T](
  name: String, rpcName: String,
  @multi args: List[ParameterMetadata[_]],
  @infer @checked resultMetadata: NewRpcMetadata.Lazy[T]
)(implicit val typeName: TypeName[T]) extends TypedMetadata[T] with MethodMetadata[T] {
  def repr(open: List[NewRpcMetadata[_]]): String =
    s"$basicRepr\n" +
      args.map(_.repr).mkString("ARGS:\n", "\n", "").indent("  ") + "\n" +
      s"RESULT: ${resultMetadata.value.repr(open)}".indent("  ")
}

case class PostMetadata[T: TypeName](
  name: String, rpcName: String,
  @reify post: POST,
  @tagged[header] @multi @verbatim headers: Vector[ParameterMetadata[String]],
  @multi body: MLinkedHashMap[String, ParameterMetadata[_]]
)(implicit val typeName: TypeName[T]) extends TypedMetadata[T] with MethodMetadata[T] {

  def repr: String =
    s"$post $basicRepr\n" +
      headers.map(_.repr).mkString("HEADERS:\n", "\n", "").indent("  ") + "\n" +
      body.iterator.map({ case (n, pm) => s"$n -> ${pm.repr}" }).mkString("BODY:\n", "\n", "").indent("  ")
}

case class ParameterMetadata[T: TypeName](
  @reifyName name: String,
  @reifyName(rpcName = true) rpcName: String,
  @reifyPosition pos: ParamPosition,
  @reifyFlags flags: ParamFlags,
  @reify @multi renames: List[renamed]
) extends TypedMetadata[T] {
  def repr: String = {
    val flagsStr = if (flags != ParamFlags.Empty) s"[$flags]" else ""
    val rpcNameStr = if (rpcName != name) s"<$rpcName>" else ""
    val posStr = s"${pos.index}:${pos.indexOfList}:${pos.indexInList}:${pos.indexInRaw}"
    val renamesStr = if (renames.nonEmpty) renames.mkString(s" renames=", ",", "") else ""
    s"$flagsStr$name$rpcNameStr@$posStr: ${TypeName.get[T]}$renamesStr"
  }
}
