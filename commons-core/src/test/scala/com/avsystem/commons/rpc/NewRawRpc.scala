package com.avsystem.commons
package rpc

import java.util.regex.Matcher

import com.avsystem.commons.annotation.AnnotationAggregate
import com.avsystem.commons.meta._
import com.avsystem.commons.misc.TypeString

trait DummyParamTag extends RpcTag with AnnotationAggregate

case class header(name: String) extends DummyParamTag {
  @rpcName(name)
  final def aggregated: List[StaticAnnotation] = reifyAggregated
}

case class renamed(int: Int, name: String) extends DummyParamTag {
  @rpcName(name)
  final def aggregated: List[StaticAnnotation] = reifyAggregated
}

case class suchMeta(intMeta: Int, strMeta: String) extends StaticAnnotation

class filter extends StaticAnnotation
class negFilter extends StaticAnnotation

sealed trait RestMethod extends RpcTag
case class POST() extends RestMethod with AnnotationAggregate {
  @rpcNamePrefix("POST_")
  final def aggregated: List[StaticAnnotation] = reifyAggregated
}
case class GET() extends RestMethod
case class PUT() extends RestMethod

case class GetterInvocation(
  @methodName name: String,
  @encoded head: String,
  @multi tail: List[String]
)

@methodTag[RestMethod]
@paramTag[DummyParamTag]
trait NewRawRpc {
  @annotated[filter] def doSomething(@annotated[filter] arg: Double): String
  @optional def doSomethingElse(arg: Double): String

  @multi
  @verbatim def fire(
    @methodName name: String,
    @optional @auxiliary ajdi: Opt[Int],
    @multi args: Map[String, String]
  ): Unit

  @multi def call(
    @methodName name: String,
    @tagged[renamed] @multi renamedArgs: => Map[String, String],
    @multi args: Map[String, String]
  ): Future[String]

  @multi def get(@composite invocation: GetterInvocation): NewRawRpc

  @multi
  @tagged[POST] def post(@methodName name: String,
    @tagged[header] @multi @verbatim headers: Vector[String],
    @multi body: MLinkedHashMap[String, String]
  ): String

  @multi def prefix(@methodName name: String): NewRawRpc
}
object NewRawRpc extends RawRpcCompanion[NewRawRpc]

object Utils {
  implicit class StringUtils(private val str: String) extends AnyVal {
    def indent(ind: String): String =
      str.replaceAll("(^|\\n)\\n*(?!$)", "$0" + Matcher.quoteReplacement(ind))
  }
}

import com.avsystem.commons.rpc.Utils._

case class DoSomethings(
  @rpcMethodMetadata doSomething: DoSomethingSignature,
  @optional @rpcMethodMetadata doSomethingElse: Opt[DoSomethingSignature]
)

@methodTag[RestMethod]
@paramTag[DummyParamTag]
case class NewRpcMetadata[T: TypeString](
  @composite doSomethings: DoSomethings,
  @multi @verbatim @rpcMethodMetadata procedures: Map[String, FireMetadata],
  @multi @rpcMethodMetadata functions: Map[String, CallMetadata[_]],
  @multi @rpcMethodMetadata getters: Map[String, GetterMetadata[_]],
  @multi @tagged[POST] @rpcMethodMetadata posters: Map[String, PostMetadata[_]],
  @multi @rpcMethodMetadata prefixers: Map[String, PrefixMetadata[_]]
) {
  def repr(open: List[NewRpcMetadata[_]]): String =
    if (open.contains(this)) "<recursive>\n" else {
      val membersStr =
        s"DO SOMETHING ELSE: ${doSomethings.doSomethingElse.nonEmpty}\n" +
          procedures.iterator.map({ case (n, v) => s"$n -> ${v.repr}" }).mkString("PROCEDURES:\n", "\n", "") + "\n" +
          functions.iterator.map({ case (n, v) => s"$n -> ${v.repr}" }).mkString("FUNCTIONS:\n", "\n", "") + "\n" +
          posters.iterator.map({ case (n, v) => s"$n -> ${v.repr}" }).mkString("POSTERS:\n", "\n", "") + "\n" +
          getters.iterator.map({ case (n, v) => s"$n -> ${v.repr(this :: open)}" }).mkString("GETTERS:\n", "\n", "") +
          prefixers.iterator.map({ case (n, v) => s"$n -> ${v.repr(this :: open)}" }).mkString("PREFIXERS:\n", "\n", "")
      s"${TypeString.of[T]}\n" + membersStr.indent("  ")
    }

  override def toString: String = repr(Nil)
}
object NewRpcMetadata extends RpcMetadataCompanion[NewRpcMetadata]

case class DoSomethingSignature(@rpcParamMetadata arg: ArgMetadata) extends TypedMetadata[String]
case class ArgMetadata() extends TypedMetadata[Double]

trait MethodMetadata[T] {
  @composite def nameInfo: NameInfo
  def typeString: TypeString[T]

  def basicRepr: String =
    s"def ${nameInfo.repr}: ${typeString.value}"
}

case class FireMetadata(
  nameInfo: NameInfo,
  @optional @auxiliary @rpcParamMetadata ajdi: Opt[ParameterMetadata[Int]],
  @multi @rpcParamMetadata args: Map[String, ParameterMetadata[_]]
) extends TypedMetadata[Unit] with MethodMetadata[Unit] {
  def typeString: TypeString[Unit] = new TypeString("void")
  def repr: String =
    s"$basicRepr\n" +
      ajdi.fold("NO AJDI")(pm => s"AJDI: ${pm.repr(Nil)}").indent("  ") + "\n" +
      args.iterator.map({ case (n, pm) => s"$n -> ${pm.repr(Nil)}" }).mkString("ARGS:\n", "\n", "").indent("  ")
}

case class CallMetadata[T](
  nameInfo: NameInfo,
  @multi @rpcTypeParamMetadata generics: List[TypeParameterMetadata],
  @tagged[renamed] @multi @rpcParamMetadata renamed: Map[String, ParameterMetadata[_]],
  @multi @rpcParamMetadata args: Map[String, ParameterMetadata[_]],
  @forTypeParams @infer("for method result") typeStringFun: List[TypeString[_]] => TypeString[T]
) extends TypedMetadata[Future[T]] with MethodMetadata[T] {
  def typeString: TypeString[T] = typeStringFun(generics.map(_.typeString))

  def repr: String =
    s"def ${nameInfo.repr}${generics.map(_.name).mkStringOrEmpty("[", ",", "]")}: ${typeString.value}\n" +
      renamed.iterator.map({ case (n, pm) => s"$n -> ${pm.repr(generics)}" }).mkString("RENAMED:\n", "\n", "").indent("  ") + "\n" +
      args.iterator.map({ case (n, pm) => s"$n -> ${pm.repr(generics)}" }).mkString("ARGS:\n", "\n", "").indent("  ")
}

case class GetterParams(
  @encoded @rpcParamMetadata head: ParameterMetadata[_],
  @multi @rpcParamMetadata tail: List[ParameterMetadata[_]]
) {
  def repr: String = (head :: tail).map(_.repr(Nil)).mkString("ARGS:\n", "\n", "")
}

case class GetterMetadata[T](
  nameInfo: NameInfo,
  @composite params: GetterParams,
  @infer @checked resultMetadata: NewRpcMetadata.Lazy[T]
)(implicit val typeString: TypeString[T]) extends TypedMetadata[T] with MethodMetadata[T] {
  def repr(open: List[NewRpcMetadata[_]]): String =
    s"$basicRepr\n" +
      params.repr.indent("  ") + "\n" +
      s"RESULT: ${resultMetadata.value.repr(open)}".indent("  ")
}

case class PostMetadata[T: TypeString](
  nameInfo: NameInfo,
  @reifyAnnot post: POST,
  @tagged[header] @multi @verbatim @rpcParamMetadata headers: Vector[ParameterMetadata[String]],
  @multi @rpcParamMetadata body: MLinkedHashMap[String, ParameterMetadata[_]]
)(implicit val typeString: TypeString[T]) extends TypedMetadata[T] with MethodMetadata[T] {

  def repr: String =
    s"$post $basicRepr\n" +
      headers.map(_.repr(Nil)).mkString("HEADERS:\n", "\n", "").indent("  ") + "\n" +
      body.iterator.map({ case (n, pm) => s"$n -> ${pm.repr(Nil)}" }).mkString("BODY:\n", "\n", "").indent("  ")
}

case class PrefixMetadata[T](
  nameInfo: NameInfo,
  @infer @checked resultMetadata: NewRpcMetadata.Lazy[T],
  @infer typeString: TypeString[T]
) extends TypedMetadata[T] with MethodMetadata[T] {
  def repr(open: List[NewRpcMetadata[_]]): String =
    s"$basicRepr\n" +
      s"RESULT: ${resultMetadata.value.repr(open)}".indent("  ")
}

case class TypeParameterMetadata(@reifyName name: String) {
  def typeString: TypeString[_] = new TypeString(name)
}

case class ParameterMetadata[T](
  @composite nameInfo: NameInfo,
  @reifyPosition pos: ParamPosition,
  @reifyFlags flags: ParamFlags,
  @reifyAnnot @multi metas: List[suchMeta],
  @isAnnotated[suchMeta] suchMeta: Boolean,
  @optional @forTypeParams @reifyAnnot annotTypeStringOpt: Opt[List[TypeString[_]] => annotTypeString[T]],
  @forTypeParams @infer typeString: List[TypeString[_]] => TypeString[T]
) extends TypedMetadata[T] {
  def repr(tparams: List[TypeParameterMetadata]): String = {
    val tparamTss = tparams.map(tp => new TypeString(tp.name))
    val flagsStr = if (flags != ParamFlags.Empty) s"[$flags]" else ""
    val posStr = s"${pos.index}:${pos.indexOfList}:${pos.indexInList}:${pos.indexInRaw}"
    val metasStr = metas.map(m => s"@suchMeta(${m.intMeta},${m.strMeta})").mkStringOrEmpty(" ", " ", "")
    val annotTsStr = annotTypeStringOpt.fold("")(ats => s" @annotTypeString[${ats(tparamTss).ts}]")
    s"$flagsStr${nameInfo.repr}@$posStr: ${typeString(tparamTss)} suchMeta=$suchMeta$metasStr$annotTsStr"
  }
}

case class NameInfo(
  @reifyName name: String,
  @reifyName(useRawName = true) rpcName: String
) {
  def repr: String = name + (if (rpcName != name) s"<$rpcName>" else "")
}

@allowIncomplete
case class PartialMetadata[T](
  @multi @rpcMethodMetadata @annotated[POST] @notAnnotated[negFilter] posts: List[PostMethod[_]]
) {
  def repr: String = posts.map(_.repr).mkString("\n")
}
object PartialMetadata extends RpcMetadataCompanion[PartialMetadata]

@allowIncomplete
case class PostMethod[T](
  @reifyName name: String,
  @multi @rpcParamMetadata @annotated[header] headerParams: List[HeaderParam[_]]
) extends TypedMetadata[T] {
  def repr: String = s"$name(${headerParams.map(_.repr).mkString(",")})"
}

case class HeaderParam[T](
  @reifyAnnot header: header
) extends TypedMetadata[T] {
  def repr: String = header.name
}
