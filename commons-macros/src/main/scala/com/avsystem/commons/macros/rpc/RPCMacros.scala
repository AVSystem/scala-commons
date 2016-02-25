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
  val FrameworkObj = c.prefix.tree
  val RunNowEC = q"$CommonsPackage.concurrent.RunNowEC"
  val RawRPCCls = tq"$FrameworkObj.RawRPC"
  val AsRawRPCObj = q"$FrameworkObj.AsRawRPC"
  val AsRawRPCCls = tq"$FrameworkObj.AsRawRPC"
  val AsRealRPCObj = q"$FrameworkObj.AsRealRPC"
  val AsRealRPCCls = tq"$FrameworkObj.AsRealRPC"
  val RawValueTpe = tq"$FrameworkObj.RawValue"
  val RPCMetadataCls = tq"$RpcPackage.RPCMetadata"
  val RPCNameAnnotType = getType(tq"$RpcPackage.RPCName")
  val RPCType = getType(tq"$RpcPackage.RPC")
  val MetadataAnnotationTpe = getType(tq"$RpcPackage.MetadataAnnotation")

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

    def rpcNameString = rpcName.decodedName.toString
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
    }.toList

    proxyables.groupBy(_.rpcName).foreach {
      case (rpcName, members) if members.size > 1 =>
        error(s"Multiple RPC methods have the same RPC name: $rpcName, you need to properly disambiguate them with @RPCName annotation")
      case _ =>
    }

    val proxyablesByType = proxyables.groupBy(_.memberType).withDefaultValue(Iterable.empty)

    proxyablesByType(Invalid).foreach { pm =>
      error(invalidProxyableMsg(pm))
    }

    val procedures = proxyablesByType(Procedure)
    val functions = proxyablesByType(Function)
    val getters = proxyablesByType(Getter)

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
      cq"(${member.rpcNameString}, $matchedArgs) => ${adjustResult(member, q"$implName.${member.method}(...$methodArgs)")}"
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
        implicit def ${c.freshName(TermName("self"))}: $AsRawRPCCls[$rpcTpe] = this

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
        implicit def ${c.freshName(TermName("self"))}: $AsRealRPCCls[$rpcTpe] = this

        def asReal($rawRpcName: $RawRPCCls) = new $rpcTpe { ..$implementations; () }
      }
     """
  }

  def materializeMetadata[T: c.WeakTypeTag]: Tree = {
    val rpcTpe = weakTypeOf[T]
    val (procedures, functions, getters) = proxyableMethods(rpcTpe)
    val methods = procedures ++ functions ++ getters

    def reifyAnnotations(s: Symbol) = {
      val trees = allAnnotations(s).iterator.map(_.tree).collect {
        case tree if tree.tpe <:< MetadataAnnotationTpe => c.untypecheck(tree)
      }.toList
      q"$ListObj(..$trees)"
    }

    def reifyParamMetadata(s: Symbol) =
      q"$RpcPackage.ParamMetadata(${s.name.decodedName.toString}, ${reifyAnnotations(s)})"

    def reifySignature(ms: MethodSymbol) =
      q"""
        $RpcPackage.Signature(
          ${ms.name.decodedName.toString},
          $ListObj(..${ms.paramLists.map(ps => q"$ListObj(..${ps.map(reifyParamMetadata)})")}),
          ${reifyAnnotations(ms)}
        )"""

    def reifyMethodMetadata(pm: ProxyableMember) = pm.memberType match {
      case Procedure => 
        q"$RpcPackage.ProcedureMetadata(${reifySignature(pm.method)})"
      case Function => 
        q"$RpcPackage.FunctionMetadata(${reifySignature(pm.method)})"
      case Getter => 
        q"$RpcPackage.GetterMetadata(${reifySignature(pm.method)}, implicitly[$RPCMetadataCls[${pm.returnType}]])"
      case Invalid => EmptyTree
    }
    
    q"""
      new $RPCMetadataCls[$rpcTpe] {
        implicit def ${c.freshName(TermName("self"))}: $RPCMetadataCls[$rpcTpe] = this
      
        def name = ${rpcTpe.typeSymbol.name.decodedName.toString}
        lazy val annotations = ${reifyAnnotations(rpcTpe.typeSymbol)}
        lazy val methodsByRpcName = $MapObj(..${methods.map(pm => q"${pm.rpcNameString} -> ${reifyMethodMetadata(pm)}")})
      }
     """
  }
}