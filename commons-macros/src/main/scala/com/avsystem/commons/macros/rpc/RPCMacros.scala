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
  val FrameworkObj = c.prefix.tree
  val RunNowEC = q"$CommonsPackage.concurrent.RunNowEC"
  val RawRPCCls = tq"$FrameworkObj.RawRPC"
  val AsRawRPCObj = q"$FrameworkObj.AsRawRPC"
  val AsRawRPCCls = tq"$FrameworkObj.AsRawRPC"
  val AsRealRPCObj = q"$FrameworkObj.AsRealRPC"
  val AsRealRPCCls = tq"$FrameworkObj.AsRealRPC"
  val RawValueCls = tq"$FrameworkObj.RawValue"
  val ArgListsCls = tq"$ListCls[$ListCls[$RawValueCls]]"
  val RealInvocationHandlerCls = tq"$FrameworkObj.RealInvocationHandler"
  val RealInvocationHandlerObj = q"$FrameworkObj.RealInvocationHandler"
  val RawInvocationHandlerCls = tq"$FrameworkObj.RawInvocationHandler"
  val RPCMetadataCls = tq"$RpcPackage.RPCMetadata"

  lazy val RPCFrameworkType = getType(tq"$RpcPackage.RPCFramework")
  lazy val RPCNameType = getType(tq"$RpcPackage.RPCName")
  lazy val RPCType = getType(tq"$RpcPackage.RPC")
  lazy val MetadataAnnotationType = getType(tq"$RpcPackage.MetadataAnnotation")
  lazy val RawValueType = getType(RawValueCls)
  lazy val RawValueLLType = getType(ArgListsCls)
  lazy val RawRPCType = getType(RawRPCCls)
  lazy val RawRPCSym = RawRPCType.typeSymbol

  def allAnnotations(tpe: Type) = {
    val ts = tpe.typeSymbol
    if (ts.isClass) ts.asClass.baseClasses.flatMap(_.annotations)
    else Nil
  }

  def hasRpcAnnot(tpe: Type) =
    allAnnotations(tpe).exists(_.tree.tpe <:< RPCType)

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

    val rpcName = (method :: method.overrides).flatMap(_.annotations).find(_.tree.tpe <:< RPCNameType).map { annot =>
      annot.tree.children.tail match {
        case List(Literal(Constant(name: String))) => TermName(name)
        case _ => c.abort(annot.tree.pos, "The argument of @RPCName must be a string literal.")
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

  def reifyList(args: List[Tree]) = q"$ListObj(..$args)"

  def reifyListPat(args: List[Tree]) = pq"$ListObj(..$args)"

  def asRawImpl[T: c.WeakTypeTag]: c.Tree = {
    val rpcTpe = weakTypeOf[T]
    val proxyables = proxyableMethods(rpcTpe)
    val implName = c.freshName(TermName("impl"))

    def methodCase(variant: Variant, member: ProxyableMember): CaseDef = {
      val paramLists = member.signature.paramLists
      val matchedArgs = reifyListPat(paramLists.map(paramList => reifyListPat(paramList.map(ps => pq"${ps.name.toTermName}"))))
      val methodArgs = paramLists.map(_.map(ps => q"$FrameworkObj.read[${ps.typeSignature}](${ps.name.toTermName})"))
      val badMethodError = s"${member.method} cannot be handled by raw ${variant.rawMethod}"
      val onFailure = q"throw new Exception($badMethodError)"
      val realInvocation = q"$implName.${member.method}(...$methodArgs)"
      cq"""
        (${member.rpcNameString}, $matchedArgs) =>
          $FrameworkObj.tryToRaw[${member.returnType},${variant.returnType}]($realInvocation,$onFailure)
        """
    }

    def defaultCase(variant: Variant): CaseDef =
      cq"_ => fail(${rpcTpe.toString}, ${variant.rawMethod.name.toString}, methodName, args)"

    def methodMatch(variant: Variant, methods: Iterable[ProxyableMember]) = {
      Match(q"(methodName, args)", (methods.map(m => methodCase(variant, m)) ++ Iterator(defaultCase(variant))).toList)
    }

    def rawImplementation(variant: Variant) =
      q"def ${variant.rawMethod.name}(methodName: String, args: $ListCls[$ListCls[$RawValueCls]]) = ${methodMatch(variant, proxyables)}"

    q"""
      new $AsRawRPCCls[$rpcTpe] with $FrameworkObj.RawRPCUtils {
        implicit def ${c.freshName(TermName("self"))}: $AsRawRPCCls[$rpcTpe] = this

        def asRaw($implName: $rpcTpe) =
          new $RawRPCCls {
            ..${variants.map(rawImplementation)}
          }
      }
     """
  }

  def tryToRaw[Real: c.WeakTypeTag, Raw: c.WeakTypeTag](real: Tree, onFailure: Tree): Tree = {
    val realTpe = weakTypeOf[Real]
    val rawTpe = weakTypeOf[Raw]
    val handlerType = getType(tq"$RealInvocationHandlerCls[$realTpe,_]")
    val expectedHandlerType = getType(tq"$RealInvocationHandlerCls[$realTpe,$rawTpe]")
    c.inferImplicitValue(handlerType) match {
      case EmptyTree => q"implicitly[$expectedHandlerType].toRaw($real)" //force normal complication error
      case handler if handler.tpe <:< expectedHandlerType => q"$handler.toRaw($real)"
      case _ => onFailure
    }
  }

  def asRealImpl[T: c.WeakTypeTag]: c.Tree = {
    val rpcTpe = weakTypeOf[T]
    val proxyables = proxyableMethods(rpcTpe)
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

    q"""
      new $AsRealRPCCls[$rpcTpe] {
        implicit def ${c.freshName(TermName("self"))}: $AsRealRPCCls[$rpcTpe] = this

        def asReal($rawRpcName: $RawRPCCls) = new $rpcTpe { ..$implementations; () }
      }
     """
  }

  def isRPC[T: c.WeakTypeTag]: Tree = {
    checkRpc(weakTypeOf[T])
    q"null"
  }

  def RPCToRaw[T: c.WeakTypeTag](ev: Tree): Tree = {
    val tpe = weakTypeOf[T]
    checkRpc(tpe)

    q"""
       new $FrameworkObj.RealInvocationHandler[$tpe,$RawRPCType] {
         implicit def ${c.freshName(TermName("self"))}: $FrameworkObj.RealInvocationHandler[$tpe,$RawRPCType] = this

         def toRaw(real: $tpe) = $AsRawRPCObj[$tpe].asRaw(real)
       }
     """
  }

  def GetHandler[T: c.WeakTypeTag](ev: Tree): Tree = {
    val tpe = weakTypeOf[T]
    checkRpc(tpe)

    q"""
      new $FrameworkObj.RawInvocationHandler[$tpe] {
        implicit def ${c.freshName(TermName("self"))}: $FrameworkObj.RawInvocationHandler[$tpe] = this

        def toReal(rawRpc: $RawRPCCls, rpcName: String, argLists: $ArgListsCls) =
          $AsRealRPCObj[$tpe].asReal(rawRpc.get(rpcName, argLists))
      }
     """
  }

  def materializeMetadata[T: c.WeakTypeTag]: Tree = {
    val rpcTpe = weakTypeOf[T]
    val proxyables = proxyableMethods(rpcTpe)

    def reifyAnnotations(s: Symbol) = {
      val trees = allAnnotations(s).iterator.map(_.tree).collect {
        case tree if tree.tpe <:< MetadataAnnotationType => c.untypecheck(tree)
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

    q"""
      new $RPCMetadataCls[$rpcTpe] {
        implicit def ${c.freshName(TermName("self"))}: $RPCMetadataCls[$rpcTpe] = this

        def name = ${rpcTpe.typeSymbol.name.decodedName.toString}
        lazy val annotations = ${reifyAnnotations(rpcTpe.typeSymbol)}
        lazy val signatures = $MapObj(..${proxyables.map(pm => q"${pm.rpcNameString} -> ${reifySignature(pm.method)}")})
        lazy val getterResults = $MapObj[String,$RPCMetadataCls[_]](
          ..${proxyables.filter(pm => hasRpcAnnot(pm.returnType)).map(pm => q"${pm.rpcNameString} -> implicitly[$RPCMetadataCls[${pm.returnType}]]")}
        )
      }
     """
  }
}
