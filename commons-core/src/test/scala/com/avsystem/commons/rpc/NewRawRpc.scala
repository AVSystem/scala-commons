package com.avsystem.commons
package rpc

import java.util.regex.Matcher

import com.avsystem.commons.annotation.AnnotationAggregate
import com.avsystem.commons.serialization.GenCodec

import scala.annotation.StaticAnnotation

trait DummyParamTag extends RpcTag with AnnotationAggregate

case class header(name: String) extends DummyParamTag {
  @rpcName(name)
  type Implied
}

case class renamed(int: Int, name: String) extends DummyParamTag {
  @rpcName(name)
  type Implied
}

case class suchMeta(intMeta: Int, strMeta: String) extends StaticAnnotation

sealed trait untagged extends DummyParamTag

sealed trait RestMethod extends RpcTag
case class POST() extends RestMethod
case class GET() extends RestMethod
case class PUT() extends RestMethod

@methodTag[RestMethod, RestMethod]
@paramTag[DummyParamTag, untagged]
trait NewRawRpc {
  def doSomething(arg: Double): String
  @optional def doSomethingElse(arg: Double): String

  @multi
  @verbatim def fire(name: String)(
    @optional @auxiliary ajdi: Opt[Int],
    @multi args: Map[String, String]): Unit

  @multi def call(name: String)(
    @tagged[renamed] @multi renamedArgs: => Map[String, String],
    @multi args: Map[String, String]): Future[String]

  @multi def get(name: String)(
    @encoded head: String, @multi tail: List[String]): NewRawRpc

  @multi
  @tagged[POST] def post(name: String)(
    @tagged[header] @multi @verbatim headers: Vector[String],
    @multi body: MLinkedHashMap[String, String]): String

  @multi def prefix(name: String): NewRawRpc
}
object NewRawRpc extends RawRpcCompanion[NewRawRpc] {
  override val implicits: this.type = this

  implicit def AsRawRealFromGenCodec[T: GenCodec]: AsRawReal[String, T] = ???
  implicit def futureAsRawRealFromGenCodec[T: GenCodec]: AsRawReal[Future[String], Future[T]] = ???
}

object Utils {
  implicit class StringUtils(private val str: String) extends AnyVal {
    def indent(ind: String): String =
      str.replaceAll("(^|\\n)\\n*(?!$)", "$0" + Matcher.quoteReplacement(ind))
  }
}

import com.avsystem.commons.rpc.Utils._

@methodTag[RestMethod, RestMethod]
@paramTag[DummyParamTag, untagged]
case class NewRpcMetadata[T: TypeName](
  doSomething: DoSomethingSignature,
  @optional doSomethingElse: Opt[DoSomethingSignature],
  @multi @verbatim procedures: Map[String, FireMetadata],
  @multi functions: Map[String, CallMetadata[_]],
  @multi getters: Map[String, GetterMetadata[_]],
  @multi @tagged[POST] posters: Map[String, PostMetadata[_]],
  @multi prefixers: Map[String, PrefixMetadata[_]]
) {
  def repr(open: List[NewRpcMetadata[_]]): String =
    if (open.contains(this)) "<recursive>\n" else {
      val membersStr =
        s"DO SOMETHING ELSE: ${doSomethingElse.nonEmpty}\n" +
          procedures.iterator.map({ case (n, v) => s"$n -> ${v.repr}" }).mkString("PROCEDURES:\n", "\n", "") + "\n" +
          functions.iterator.map({ case (n, v) => s"$n -> ${v.repr}" }).mkString("FUNCTIONS:\n", "\n", "") + "\n" +
          posters.iterator.map({ case (n, v) => s"$n -> ${v.repr}" }).mkString("POSTERS:\n", "\n", "") + "\n" +
          getters.iterator.map({ case (n, v) => s"$n -> ${v.repr(this :: open)}" }).mkString("GETTERS:\n", "\n", "") +
          prefixers.iterator.map({ case (n, v) => s"$n -> ${v.repr(this :: open)}" }).mkString("PREFIXERS:\n", "\n", "")
      s"${TypeName.get[T]}\n" + membersStr.indent("  ")
    }

  override def toString: String = repr(Nil)
}
object NewRpcMetadata extends RpcMetadataCompanion[NewRpcMetadata]

case class DoSomethingSignature(arg: ArgMetadata) extends TypedMetadata[String]
case class ArgMetadata() extends TypedMetadata[Double]

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
  @encoded head: ParameterMetadata[_],
  @multi tail: List[ParameterMetadata[_]],
  @infer @checked resultMetadata: NewRpcMetadata.Lazy[T]
)(implicit val typeName: TypeName[T]) extends TypedMetadata[T] with MethodMetadata[T] {
  def repr(open: List[NewRpcMetadata[_]]): String =
    s"$basicRepr\n" +
      (head :: tail).map(_.repr).mkString("ARGS:\n", "\n", "").indent("  ") + "\n" +
      s"RESULT: ${resultMetadata.value.repr(open)}".indent("  ")
}

case class PostMetadata[T: TypeName](
  name: String, rpcName: String,
  @reifyAnnot post: POST,
  @tagged[header] @multi @verbatim headers: Vector[ParameterMetadata[String]],
  @multi body: MLinkedHashMap[String, ParameterMetadata[_]]
)(implicit val typeName: TypeName[T]) extends TypedMetadata[T] with MethodMetadata[T] {

  def repr: String =
    s"$post $basicRepr\n" +
      headers.map(_.repr).mkString("HEADERS:\n", "\n", "").indent("  ") + "\n" +
      body.iterator.map({ case (n, pm) => s"$n -> ${pm.repr}" }).mkString("BODY:\n", "\n", "").indent("  ")
}

case class PrefixMetadata[T](
  name: String, rpcName: String,
  @infer @checked resultMetadata: NewRpcMetadata.Lazy[T],
  @infer typeName: TypeName[T]
) extends TypedMetadata[T] with MethodMetadata[T] {
  def repr(open: List[NewRpcMetadata[_]]): String =
    s"$basicRepr\n" +
      s"RESULT: ${resultMetadata.value.repr(open)}".indent("  ")
}

case class ParameterMetadata[T: TypeName](
  @reifyName name: String,
  @reifyName(rpcName = true) rpcName: String,
  @reifyPosition pos: ParamPosition,
  @reifyFlags flags: ParamFlags,
  @reifyAnnot @multi metas: List[suchMeta],
  @hasAnnot[suchMeta] suchMeta: Boolean
) extends TypedMetadata[T] {
  def repr: String = {
    val flagsStr = if (flags != ParamFlags.Empty) s"[$flags]" else ""
    val rpcNameStr = if (rpcName != name) s"<$rpcName>" else ""
    val posStr = s"${pos.index}:${pos.indexOfList}:${pos.indexInList}:${pos.indexInRaw}"
    val metasStr = if (metas.nonEmpty) metas.mkString(s",metas=", ",", "") else ""
    s"$flagsStr$name$rpcNameStr@$posStr: ${TypeName.get[T]} suchMeta=$suchMeta$metasStr"
  }
}
