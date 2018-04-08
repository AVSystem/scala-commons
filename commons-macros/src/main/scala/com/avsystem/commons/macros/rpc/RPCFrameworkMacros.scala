package com.avsystem.commons
package macros.rpc

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.reflect.macros.blackbox

class RPCFrameworkMacros(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  import c.universe._

  val FrameworkObj = c.prefix.tree
  val RpcPackage = q"$CommonsPackage.rpc"
  val RunNowEC = q"$CommonsPackage.concurrent.RunNowEC"
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
  lazy val RPCNameType = getType(tq"$RpcPackage.RPCName")
  lazy val RPCType = getType(tq"$RpcPackage.RPC")
  lazy val MetadataAnnotationType = getType(tq"$RpcPackage.MetadataAnnotation")
  lazy val RawValueType = getType(RawValueCls)
  lazy val RawValueLLType = getType(ArgListsCls)
  lazy val RawRPCType = getType(RawRPCCls)
  lazy val RawRPCSym = RawRPCType.typeSymbol
  lazy val RPCCompanionSym = RPCFrameworkType.member(TypeName("RPCCompanion"))

  def hasRpcAnnot(tpe: Type) =
    allAnnotations(tpe.typeSymbol).exists(_.tpe <:< RPCType)

  case class Variant(rawMethod: MethodSymbol, returnType: Type)

  lazy val variants = RawRPCType.members.filter(s => s.isTerm && s.isAbstract)
    .map { s =>
      if (s.isMethod) {
        val m = s.asMethod
        val sig = m.typeSignatureIn(RawRPCType)
        if (sig.typeParams.nonEmpty) {
          abort(s"Bad signature ($m): RPC variant cannot be generic")
        }
        val returnType = sig.paramLists match {
          case List(List(rpcNameParam, argListsParam))
            if rpcNameParam.typeSignature =:= typeOf[String] && argListsParam.typeSignature =:= RawValueLLType =>
            sig.finalResultType
          case _ =>
            abort(s"Bad signature ($m): RPC variant must take two parameters of types String and List[List[RawValue]]")
        }
        Variant(m, returnType)
      } else {
        abort("All abstract members in RawRPC must be methods that take two parameters of types String and List[List[RawValue]]")
      }
    }.toList

  case class ProxyableMember(method: MethodSymbol, signature: Type) {
    val returnType = signature.finalResultType
    val typeParams = signature.typeParams
    val paramLists = signature.paramLists

    val rpcName = allAnnotations(method).find(_.tpe <:< RPCNameType).map { annot =>
      annot.children.tail match {
        case List(StringLiteral(name)) => TermName(name)
        case p :: _ => c.abort(p.pos, "The argument of @RPCName must be a string literal.")
        case _ => c.abort(annot.pos, "No name argument found in @RPCName annotation")
      }
    }.getOrElse(method.name)

    def rpcNameString = rpcName.decodedName.toString
  }

  def checkRpc(tpe: Type): Unit = {
    if (!hasRpcAnnot(tpe) || !tpe.typeSymbol.isClass || tpe <:< typeOf[AnyVal] || tpe <:< typeOf[Null]) {
      abort(s"RPC type must be a trait or abstract class annotated as @RPC, $tpe is not.")
    }
  }

  def proxyableMethods(tpe: Type) = {
    checkRpc(tpe)

    val proxyables = tpe.members.filter(m => m.isTerm && m.isAbstract).map { m =>
      val signature = m.typeSignatureIn(tpe)
      if (!m.isMethod || signature.typeParams.nonEmpty) {
        abort(s"All abstract members in RPC interface must be non-generic methods, $m in $tpe is not.")
      }
      ProxyableMember(m.asMethod, signature)
    }.toList

    proxyables.groupBy(_.rpcName).foreach {
      case (rpcName, members) if members.size > 1 =>
        error(s"Multiple RPC methods have the same RPC name: $rpcName, you need to properly disambiguate them with @RPCName annotation")
      case _ =>
    }

    if (proxyables.isEmpty) {
      warning(s"$tpe has no abstract members that could represent remote methods.")
    }

    proxyables
  }

  def inferOrMaterialize[T: c.WeakTypeTag](typeClass: Tree, materialize: (Type, List[ProxyableMember]) => Tree): Tree = {
    val rpcTpe = weakTypeOf[T]
    if (c.macroApplication.symbol.isImplicit) {
      val instanceTpe = getType(tq"$typeClass[$rpcTpe]")
      c.inferImplicitValue(instanceTpe, withMacrosDisabled = true) match {
        case EmptyTree => materialize(rpcTpe, proxyableMethods(rpcTpe))
        case tree => tree
      }
    }
    else materialize(rpcTpe, proxyableMethods(rpcTpe))
  }

  def reifyList(args: List[Tree]) = q"$ListObj(..$args)"

  def reifyListPat(args: List[Tree]) = pq"$ListObj(..$args)"

  def asRawImpl[T: c.WeakTypeTag]: c.Tree =
    inferOrMaterialize[T](AsRawRPCCls, materializeAsRaw)

  def materializeAsRaw(rpcTpe: Type, proxyables: List[ProxyableMember]): Tree = {
    val implName = c.freshName(TermName("impl"))

    def methodCase(variant: Variant, member: ProxyableMember): Option[CaseDef] = {
      val paramLists = member.signature.paramLists
      val matchedArgs = reifyListPat(paramLists.map(paramList => reifyListPat(paramList.map(ps => pq"${ps.name.toTermName}"))))
      val methodArgs = paramLists.map(_.map(ps => q"$FrameworkObj.read[${ps.typeSignature}](${ps.name.toTermName})"))
      val realInvocation = q"$implName.${member.method}(...$methodArgs)"
      val handlerType = getType(tq"$RealInvocationHandlerCls[${member.returnType},_]")
      val expectedHandlerType = getType(tq"$RealInvocationHandlerCls[${member.returnType},${variant.returnType}]")

      c.inferImplicitValue(handlerType) match {
        case EmptyTree =>
          Some(cq"(${member.rpcNameString}, $matchedArgs) => implicitly[$expectedHandlerType].toRaw($realInvocation)") //force normal compilation error
        case handler if handler.tpe <:< expectedHandlerType =>
          Some(cq"""(${member.rpcNameString}, $matchedArgs) => $FrameworkObj.tryToRaw[${member.returnType},${variant.returnType}]($realInvocation)""")
        case _ => None
      }
    }

    def defaultCase(variant: Variant): CaseDef =
      cq"_ => fail(${rpcTpe.toString}, ${variant.rawMethod.name.toString}, methodName, args)"

    def methodMatch(variant: Variant, methods: Iterable[ProxyableMember]) = {
      Match(q"(methodName, args)",
        (methods.flatMap(m => methodCase(variant, m)) ++ Iterator(defaultCase(variant))).toList
      )
    }

    def rawImplementation(variant: Variant) =
      q"def ${variant.rawMethod.name}(methodName: String, args: $ListCls[$ListCls[$RawValueCls]]) = ${methodMatch(variant, proxyables)}"

    // Thanks to `Materialized` trait, the implicit "self" has more specific type than a possible implicit
    // that this macro invocation is assigned to.
    q"""
      new $AsRawRPCCls[$rpcTpe] with $FrameworkObj.RawRPCUtils with $MaterializedCls {
        private implicit def ${c.freshName(TermName("self"))}: $AsRawRPCCls[$rpcTpe] with $MaterializedCls = this

        def asRaw($implName: $rpcTpe) =
          new $RawRPCCls {
            ..${variants.map(rawImplementation)}
          }
      }
     """
  }

  def tryToRaw[Real: c.WeakTypeTag, Raw: c.WeakTypeTag](real: Tree): Tree = {
    val realTpe = weakTypeOf[Real]
    val rawTpe = weakTypeOf[Raw]
    val expectedHandlerType = getType(tq"$RealInvocationHandlerCls[$realTpe,$rawTpe]")
    c.inferImplicitValue(expectedHandlerType) match {
      case EmptyTree => q"implicitly[$expectedHandlerType].toRaw($real)" //force normal compilation error
      case handler => q"$handler.toRaw($real)"
    }
  }

  def asRealImpl[T: c.WeakTypeTag]: c.Tree = inferOrMaterialize[T](AsRealRPCCls, materializeAsReal)

  def materializeAsReal(rpcTpe: Type, proxyables: List[ProxyableMember]): Tree = {
    val rawRpcName = c.freshName(TermName("rawRpc"))

    val implementations = proxyables.map { m =>
      val methodName = m.method.name
      val paramLists = m.paramLists
      val params = paramLists.map(_.map { ps =>
        val implicitFlag = if (ps.isImplicit) Flag.IMPLICIT else NoFlags
        ValDef(Modifiers(Flag.PARAM | implicitFlag), ps.name.toTermName, TypeTree(ps.typeSignature), EmptyTree)
      })

      val args = reifyList(paramLists.map(paramList =>
        reifyList(paramList.map(ps => q"$FrameworkObj.write[${ps.typeSignature}](${ps.name.toTermName})"))))

      q"def $methodName(...$params) = implicitly[$RawInvocationHandlerCls[${m.returnType}]].toReal($rawRpcName, ${m.rpcNameString}, $args)"
    }

    // Thanks to `Materialized` trait, the implicit "self" has more specific type than a possible implicit
    // that this macro invocation is assigned to.
    q"""
      new $AsRealRPCCls[$rpcTpe] with $MaterializedCls {
        private implicit def ${c.freshName(TermName("self"))}: $AsRealRPCCls[$rpcTpe] with $MaterializedCls = this

        def asReal($rawRpcName: $RawRPCCls) = new $rpcTpe { ..$implementations; () }
      }
     """
  }

  def isRPC[T: c.WeakTypeTag]: Tree = {
    checkRpc(weakTypeOf[T])
    q"null"
  }

  def getterRealHandler[T: c.WeakTypeTag](ev: Tree): Tree = {
    val tpe = weakTypeOf[T]
    checkRpc(tpe)
    q"new $FrameworkObj.GetterRealHandler[$tpe]"
  }

  def getterRawHandler[T: c.WeakTypeTag](ev: Tree): Tree = {
    val tpe = weakTypeOf[T]
    checkRpc(tpe)
    q"new $FrameworkObj.GetterRawHandler[$tpe]"
  }

  def metadataImpl[T: c.WeakTypeTag]: Tree = inferOrMaterialize[T](RPCMetadataCls, materializeMetadata)

  def materializeMetadata(rpcTpe: Type, proxyables: List[ProxyableMember]): Tree = {
    def reifyAnnotations(s: Symbol) = {
      val trees = allAnnotations(s).collect {
        case tree if tree.tpe <:< MetadataAnnotationType => c.untypecheck(tree)
      }
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

    q"""
      new $RPCMetadataCls[$rpcTpe] with $MaterializedCls {
        private implicit def ${c.freshName(TermName("self"))}: $RPCMetadataCls[$rpcTpe] with $MaterializedCls = this

        def name = ${rpcTpe.typeSymbol.name.decodedName.toString}
        lazy val annotations = ${reifyAnnotations(rpcTpe.typeSymbol)}
        lazy val signatures = $MapObj[String,$FrameworkObj.Signature](
          ..${proxyables.map(pm => q"${pm.rpcNameString} -> ${reifySignature(pm)}")}
        )
        lazy val getterResults = $MapObj[String,$RPCMetadataCls[_]](
          ..${proxyables.filter(pm => hasRpcAnnot(pm.returnType)).map(pm => q"${pm.rpcNameString} -> implicitly[$RPCMetadataCls[${pm.returnType}]]")}
        )
      }
     """
  }

  def fullInfoImpl[T: c.WeakTypeTag]: Tree = {
    val rpcTpe = weakTypeOf[T]
    val proxyables = proxyableMethods(rpcTpe)
    val fullRpcInfoTpe = getType(tq"$FullRPCInfoCls[$rpcTpe]").dealias
    if (!fullRpcInfoTpe.typeSymbol.isClass) {
      abort(s"Cannot materialize FullRPCInfo in $FrameworkObj because this framework does not define FullRPCInfo as trait or class")
    }
    q"""
       new $fullRpcInfoTpe {
         lazy val asRealRPC = ${materializeAsReal(rpcTpe, proxyables)}
         lazy val asRawRPC = ${materializeAsRaw(rpcTpe, proxyables)}
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
