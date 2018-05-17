package com.avsystem.commons
package macros.rpc

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.reflect.macros.blackbox

/**
  * The "legacy" macros for RPC based on `RPCFramework`.
  * Superseded by generalized, "framework-less" RPC macro engine, [[RPCMacros]].
  */
class RPCFrameworkMacros(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  import c.universe._

  val FrameworkObj = c.prefix.tree
  val RpcPackage = q"$CommonsPkg.rpc"
  val RawRPCCls = tq"$FrameworkObj.RawRPC"
  val AsRawRPCObj = q"$FrameworkObj.AsRawRPC"
  val AsRawRPCCls = tq"$FrameworkObj.AsRawRPC"
  val AsRealRPCObj = q"$FrameworkObj.AsRealRPC"
  val AsRealRPCCls = tq"$FrameworkObj.AsRealRPC"
  val RawValueCls = tq"$FrameworkObj.RawValue"
  val ArgListsCls = tq"$ListCls[$ListCls[$RawValueCls]]"
  val RealInvocationHandlerCls = tq"$FrameworkObj.RealInvocationHandler"
  val RawInvocationHandlerCls = tq"$FrameworkObj.RawInvocationHandler"
  val RPCMetadataObj = q"$FrameworkObj.RPCMetadata"
  val RPCMetadataCls = tq"$FrameworkObj.RPCMetadata"
  val FullRPCInfoCls = tq"$FrameworkObj.FullRPCInfo"

  lazy val RPCFrameworkType = getType(tq"$RpcPackage.RPCFramework")
  lazy val RpcNameType = getType(tq"$RpcPackage.rpcName")
  lazy val RpcNameNameSym: Symbol = RpcNameType.member(TermName("name"))
  lazy val MetadataAnnotationType = getType(tq"$RpcPackage.MetadataAnnotation")
  lazy val RawValueType = getType(RawValueCls)
  lazy val RawValueLLType = getType(ArgListsCls)
  lazy val RawRPCType = getType(RawRPCCls)
  lazy val RawRPCSym = RawRPCType.typeSymbol
  lazy val RPCCompanionSym = RPCFrameworkType.member(TypeName("RPCCompanion"))

  case class ProxyableMember(method: MethodSymbol, signature: Type) {
    val returnType = signature.finalResultType
    val typeParams = signature.typeParams
    val paramLists = signature.paramLists

    lazy val rpcName: TermName =
      findAnnotation(method, RpcNameType).fold(method.name)(_.findArg(RpcNameNameSym) match {
        case StringLiteral(n) => TermName(n)
        case p => c.abort(p.pos, "The argument of @RPCName must be a string literal.")
      })

    def rpcNameString = rpcName.decodedName.toString
  }

  def proxyableMethods(tpe: Type) = {
    val proxyables = tpe.members.filter(m => m.isTerm && m.isAbstract).map { m =>
      val signature = m.typeSignatureIn(tpe)
      if (!m.isMethod || signature.typeParams.nonEmpty) {
        abort(s"All abstract members in RPC interface must be non-generic methods, $m in $tpe is not.")
      }
      ProxyableMember(m.asMethod, signature)
    }.toList

    proxyables.groupBy(_.rpcName).foreach {
      case (rpcName, members) if members.size > 1 =>
        error(s"Multiple RPC methods have the same RPC name: $rpcName, you need to properly disambiguate them with @rpcName annotation")
      case _ =>
    }

    if (proxyables.isEmpty) {
      warning(s"$tpe has no abstract members that could represent remote methods.")
    }

    proxyables
  }

  def asRealImpl[T: WeakTypeTag]: Tree =
    q"$RpcPackage.AsReal.materializeForRpc[${weakTypeOf[T]},$RawRPCType]"

  def asRawImpl[T: WeakTypeTag]: Tree =
    q"$RpcPackage.AsRaw.materializeForRpc[${weakTypeOf[T]},$RawRPCType]"

  def asRealRawImpl[T: WeakTypeTag]: Tree =
    q"$RpcPackage.AsRealRaw.materializeForRpc[${weakTypeOf[T]},$RawRPCType]"

  def hasMetadata(tpe: Type): Boolean =
    c.inferImplicitValue(getType(tq"$RPCMetadataCls[$tpe]")) != EmptyTree

  def metadataImpl[T: WeakTypeTag]: Tree = {
    val rpcTpe = weakTypeOf[T]
    materializeMetadata(rpcTpe, proxyableMethods(rpcTpe))
  }

  def materializeMetadata(rpcTpe: Type, proxyables: List[ProxyableMember]): Tree = {
    def reifyAnnotations(s: Symbol) = {
      val trees = allAnnotations(s, MetadataAnnotationType).map(a => c.untypecheck(a.tree))
      q"$ListObj(..$trees)"
    }

    def reifyParamMetadata(s: Symbol) =
      q"$FrameworkObj.ParamMetadata(${s.name.decodedName.toString}, ${reifyAnnotations(s)}, implicitly[$FrameworkObj.ParamTypeMetadata[${s.typeSignature}]])"

    def reifySignature(pm: ProxyableMember) =
      q"""
        $FrameworkObj.Signature(
          ${pm.method.name.decodedName.toString},
          $ListObj(..${pm.paramLists.map(ps => q"$ListObj(..${ps.map(reifyParamMetadata)})")}),
          implicitly[$FrameworkObj.ResultTypeMetadata[${pm.returnType}]],
          ${reifyAnnotations(pm.method)}
        )
       """

    val selfName = c.freshName(TermName("self"))
    val getterResults = proxyables.collect {
      case pm if pm.returnType =:= rpcTpe => q"${pm.rpcNameString} -> $selfName"
      case pm if hasMetadata(pm.returnType) => q"${pm.rpcNameString} -> implicitly[$RPCMetadataCls[${pm.returnType}]]"
    }

    q"""
      new $RPCMetadataCls[$rpcTpe] with $MaterializedCls {
        private implicit def $selfName: $RPCMetadataCls[$rpcTpe] with $MaterializedCls = this

        def name = ${rpcTpe.typeSymbol.name.decodedName.toString}
        lazy val annotations = ${reifyAnnotations(rpcTpe.typeSymbol)}
        lazy val signatures = $MapObj[String,$FrameworkObj.Signature](
          ..${proxyables.map(pm => q"${pm.rpcNameString} -> ${reifySignature(pm)}")}
        )
        lazy val getterResults = $MapObj[String,$RPCMetadataCls[_]](..$getterResults)
      }
     """
  }

  def fullInfoImpl[T: WeakTypeTag]: Tree = {
    val rpcTpe = weakTypeOf[T]
    val proxyables = proxyableMethods(rpcTpe)
    val fullRpcInfoTpe = getType(tq"$FullRPCInfoCls[$rpcTpe]").dealias
    if (!fullRpcInfoTpe.typeSymbol.isClass) {
      abort(s"Cannot materialize FullRPCInfo in $FrameworkObj because this framework does not define FullRPCInfo as trait or class")
    }
    q"""
       new $fullRpcInfoTpe {
         lazy val asRealRPC = $FrameworkObj.materializeAsReal[$rpcTpe]
         lazy val asRawRPC = $FrameworkObj.materializeAsRaw[$rpcTpe]
         lazy val metadata = ${materializeMetadata(rpcTpe, proxyables)}
       }
     """
  }

  /**
    * Macro that extracts `AsRealRPC`, `AsRawRPC` and `RPCMetadata` from a companion of RPC interface that extends
    * `RPCCompanion`. Macro is necessary to make sure that each callsite keeps all the type information needed
    * for ScalaJS DCE to do its job properly.
    */
  def typeClassFromFullInfo: Tree = {
    val TypeRef(frameworkTpe, _, List(rpcTpe)) = c.prefix.actualType.baseType(RPCCompanionSym)
    q"(${c.prefix}.fullRpcInfo: $frameworkTpe#FullRPCInfo[$rpcTpe]).${c.macroApplication.symbol.name.toTermName}"
  }
}
