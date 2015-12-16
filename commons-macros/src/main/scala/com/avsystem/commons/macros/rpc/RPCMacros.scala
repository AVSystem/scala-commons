package com.avsystem.commons
package macros.rpc

import com.avsystem.commons.macros.MacroCommons

import scala.reflect.macros.blackbox

/**
  * Author: ghik
  * Created: 01/12/15.
  */
class RPCMacros(val c: blackbox.Context) extends MacroCommons {

  import c.universe._

  val RpcPackage = q"$CommonsPackage.rpc"
  val RPCFrameworkType = getType(tq"$RpcPackage.RPCFramework")

  val FrameworkObj = c.prefix.tree match {
    case Select(framework, _) if framework.tpe <:< RPCFrameworkType => framework
    case t => abort(s"Bad RPC macro prefix: $t")
  }

  val RunNowEC = q"$CommonsPackage.concurrent.RunNowEC"
  val RawRPCCls = tq"$FrameworkObj.RawRPC"
  val AsRawRPCObj = q"$FrameworkObj.AsRawRPC"
  val AsRawRPCCls = tq"$FrameworkObj.AsRawRPC"
  val AsRealRPCObj = q"$FrameworkObj.AsRealRPC"
  val AsRealRPCCls = tq"$FrameworkObj.AsRealRPC"
  val RawValueTpe = tq"$FrameworkObj.RawValue"
  val RPCNameAnnotType = getType(tq"$RpcPackage.RPCName")
  val RPCType = getType(tq"$RpcPackage.RPC")

  def allAnnotations(tpe: Type) = {
    val ts = tpe.typeSymbol
    if (ts.isClass) ts.asClass.baseClasses.flatMap(_.annotations)
    else Nil
  }

  def hasRpcAnnot(tpe: Type) =
    allAnnotations(tpe).exists(_.tree.tpe <:< RPCType)

  sealed trait MemberType

  case object Procedure extends MemberType

  case object Function extends MemberType

  case object Getter extends MemberType

  case object Invalid extends MemberType

  case class ProxyableMember(method: MethodSymbol, signature: Type) {
    val returnType = signature.finalResultType
    val typeParams = signature.typeParams
    val paramLists = signature.paramLists

    val memberType =
      if (returnType =:= typeOf[Unit]) Procedure
      else if (returnType.typeSymbol == FutureSym) Function
      else if (hasRpcAnnot(returnType)) Getter
      else Invalid

    val rpcName = (method :: method.overrides).flatMap(_.annotations).find(_.tree.tpe <:< RPCNameAnnotType).map { annot =>
      annot.tree.children.tail match {
        case List(Literal(Constant(name: String))) => TermName(name)
        case _ => c.abort(annot.tree.pos, "The argument of @RPCName must be a string literal.")
      }
    }.getOrElse(method.name)
  }

  def proxyableMethods(tpe: Type) = {
    def invalidProxyableMsg(pm: ProxyableMember): String =
      s"All abstract members in RPC interface must be non-generic methods that return Unit, Future or another RPC interface, ${pm.method} in $tpe does not."

    if (!hasRpcAnnot(tpe) || !tpe.typeSymbol.isClass || tpe <:< typeOf[AnyVal] || tpe <:< typeOf[Null]) {
      abort(s"RPC type must be a trait or abstract class annotated as @RPC, $tpe is not.")
    }

    val proxyables = tpe.members.filter(m => m.isTerm && m.isAbstract).map { m =>
      val signature = m.typeSignatureIn(tpe)
      ProxyableMember(m.asMethod, signature)
    }.groupBy(_.memberType).withDefaultValue(Iterable.empty)

    proxyables(Invalid).foreach { pm =>
      error(invalidProxyableMsg(pm))
    }

    def validateShapes(members: Iterable[ProxyableMember]): Unit = {
      def shape(pm: ProxyableMember) = (pm.rpcName, pm.paramLists.map(_.map(_ => ())))
      members.groupBy(shape).values.filter(_.size > 1).foreach { overloads =>
        val name = overloads.head.method.name.decodedName.toString
        abort(s"Overloaded variants of $name in $tpe have the same shape " +
          s"(the same number of arguments in every parameter list)\n" +
          s"You can give each variant different @RPCName to resolve such conflicts.")
      }
    }

    val procedures = proxyables(Procedure)
    val functions = proxyables(Function)
    val getters = proxyables(Getter)

    validateShapes(procedures)
    validateShapes(functions)
    validateShapes(getters)

    if (proxyables.isEmpty) {
      warning(s"$tpe has no abstract members that could represent remote methods.")
    }

    (procedures, functions, getters)
  }

  def reifyList(args: List[Tree]) = q"$ListObj(..$args)"

  def reifyListPat(args: List[Tree]) = pq"$ListObj(..$args)"

  def asRawImpl[T: c.WeakTypeTag]: c.Tree = {
    val rpcTpe = weakTypeOf[T]
    val (procedures, functions, getters) = proxyableMethods(rpcTpe)

    val implName = c.freshName(TermName("impl"))

    def methodCase(member: ProxyableMember) = {
      val paramLists = member.signature.paramLists
      val matchedArgs = reifyListPat(paramLists.map(paramList => reifyListPat(paramList.map(ps => pq"${ps.name.toTermName}"))))
      val methodArgs = paramLists.map(_.map(ps => q"$FrameworkObj.read[${ps.typeSignature}](${ps.name.toTermName})"))
      cq"(${member.rpcName.toString}, $matchedArgs) => ${adjustResult(member, q"$implName.${member.method}(...$methodArgs)")}"
    }

    def adjustResult(member: ProxyableMember, result: Tree) = member.memberType match {
      case Procedure => result
      case Function =>
        val TypeRef(_, _, List(resultTpe)) = member.returnType
        q"$result.map($FrameworkObj.write[$resultTpe])($RunNowEC)"
      case Getter =>
        q"$AsRawRPCObj[${member.returnType}].asRaw($result)"
      case Invalid =>
        sys.error("Not a proxyable member")
    }

    def defaultCase(memberType: MemberType) = cq"_ => fail(${rpcTpe.toString}, ${memberType.toString.toLowerCase}, methodName, args)"
    def methodMatch(methods: Iterable[ProxyableMember], memberType: MemberType) =
      Match(q"(methodName, args)", (methods.iterator.map(m => methodCase(m)) ++ Iterator(defaultCase(memberType))).toList)

    q"""
      new $AsRawRPCCls[$rpcTpe] {
        def asRaw($implName: $rpcTpe) =
          new $RawRPCCls {
            def fire(methodName: String, args: $ListCls[$ListCls[$FrameworkObj.RawValue]]) = ${methodMatch(procedures, Procedure)}

            def call(methodName: String, args: $ListCls[$ListCls[$FrameworkObj.RawValue]]) = ${methodMatch(functions, Function)}

            def get(methodName: String, args: $ListCls[$ListCls[$FrameworkObj.RawValue]]) = ${methodMatch(getters, Getter)}
          }
      }
     """
  }

  def asRealImpl[T: c.WeakTypeTag]: c.Tree = {
    val rpcTpe = weakTypeOf[T]
    val (procedures, functions, getters) = proxyableMethods(rpcTpe)
    val methods = procedures ++ functions ++ getters
    val rawRpcName = c.freshName(TermName("rawRpc"))

    val implementations = methods.map { m =>
      val methodName = m.method.name

      val rawRpcMethod = TermName(m.memberType match {
        case Procedure => "fire"
        case Function => "call"
        case Getter => "get"
        case Invalid => sys.error("Not a proxyable member")
      })

      val paramLists = m.paramLists
      val params = paramLists.map(_.map { ps =>
        val implicitFlag = if (ps.isImplicit) Flag.IMPLICIT else NoFlags
        ValDef(Modifiers(Flag.PARAM | implicitFlag), ps.name.toTermName, TypeTree(ps.typeSignature), EmptyTree)
      })

      val args = reifyList(paramLists.map(paramList =>
        reifyList(paramList.map(ps => q"$FrameworkObj.write[${ps.typeSignature}](${ps.name.toTermName})"))))

      val body = q"$rawRpcName.$rawRpcMethod(${m.rpcName.toString}, $args)"
      val adjustedBody = m.memberType match {
        case Procedure => body
        case Function =>
          val TypeRef(_, _, List(resultTpe)) = m.returnType
          q"$body.map($FrameworkObj.read[$resultTpe])($RunNowEC)"
        case Getter =>
          q"$AsRealRPCObj[${m.returnType}].asReal($body)"
        case Invalid =>
          sys.error("Not a proxyable member")
      }

      q"def $methodName(...$params) = $adjustedBody"
    }

    q"""
      new $AsRealRPCCls[$rpcTpe] {
        def asReal($rawRpcName: $RawRPCCls) = new $rpcTpe { ..$implementations; () }
      }
     """
  }
}