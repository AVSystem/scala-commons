package com.avsystem.commons
package macros.rpc

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.macros.blackbox

class RPCMacros(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  import c.universe._

  val RpcPackage = q"$CommonsPackage.rpc"
  val RPCNameType: Type = getType(tq"$RpcPackage.RPCName")
  val RPCNameNameSym: Symbol = RPCNameType.member(TermName("name"))
  val AsRealCls = tq"$RpcPackage.AsReal"
  val AsRawCls = tq"$RpcPackage.AsRaw"

  def rpcNameStr(sym: Symbol): String = allAnnotations(sym)
    .find(_.tpe <:< RPCNameType)
    .map { annot =>
      findAnnotationArg(annot, RPCNameNameSym) match {
        case StringLiteral(name) => name
        case p => c.abort(p.pos, "The `name` argument of @RPCName must be a string literal.")
      }
    }.getOrElse(sym.nameStr)

  abstract class RpcMethod {
    val owner: Type
    val symbol: Symbol
    val name: TermName = symbol.name.toTermName
    val nameStr: String = name.decodedName.toString
    val encodedNameStr: String = name.encodedName.toString

    protected def failMsg(explanation: String) =
      s"Can't proxy $symbol because $explanation"

    ensure(symbol.isMethod, failMsg("it's not a method"))

    val sig: Type = symbol.typeSignatureIn(owner)
    ensure(sig.typeParams.isEmpty, failMsg("it has type parameter(s)")) // can we relax this?

    val resultType: Type = sig.finalResultType
  }

  case class RawMethod(owner: Type, symbol: Symbol) extends RpcMethod {
    val paramType: Type = {
      def failSigMsg = failMsg("it has wrong signature: it must take exactly two parameters: " +
        "RPC name and a map of encoded real parameters")
      sig.paramLists match {
        case List(List(nameParam, argsParam)) =>
          val argsTpe = argsParam.typeSignature.dealias
          val validSig = nameParam.typeSignature =:= typeOf[String] && argsTpe.typeSymbol == MapSym
          ensure(validSig, failSigMsg)
          argsTpe.typeArgs(1)
        case _ =>
          abort(failSigMsg)
      }
    }

    def rawImpl(caseDefs: List[CaseDef]): Tree =
      q"""
        def $name(rpcName: $StringCls, args: $MapCls[$StringCls,$paramType]): $resultType =
          rpcName match {
            case ..$caseDefs
            case _ => $RpcPackage.RpcUtils.unknownRpc(rpcName, $nameStr)
          }
       """
  }

  case class RealMethod(owner: Type, symbol: Symbol) extends RpcMethod {
    val rpcName: String = rpcNameStr(symbol)

    val paramLists: List[List[Symbol]] = sig.paramLists
    paramLists.flatten.groupBy(rpcNameStr).foreach {
      case (_, List(_)) =>
      case (n, _) => abort(s"Multiple parameters of RPC $nameStr have the same @RPCName $n")
    }

    def paramDecls: List[List[ValDef]] = paramLists.map(_.map { ps =>
      val implicitFlag = if (ps.isImplicit) Flag.IMPLICIT else NoFlags
      ValDef(Modifiers(Flag.PARAM | implicitFlag), ps.name.toTermName, TypeTree(ps.typeSignature), EmptyTree)
    })

    def findMapping(rawMethods: List[RawMethod], forAsRaw: Boolean): Mapping = {
      val mappings = rawMethods.flatMap { rawMethod =>
        val resultConvTpe = getType(tq"${if (forAsRaw) AsRawCls else AsRealCls}[$resultType,${rawMethod.resultType}]")

        def collectParamConvs(params: List[List[Symbol]]): List[List[TermName]] = params match {
          case Nil => Nil
          case Nil :: tail => Nil :: collectParamConvs(tail)
          case (param :: rest) :: tail =>
            val paramNameStr = param.name.decodedName.toString
            val problemClue = s"Problem with parameter $paramNameStr of RPC $nameStr: "
            ensure(!param.asTerm.isByNameParam, s"${problemClue}encoded RPC parameters cannot be passed by name")
            val paramTpe = actualParamType(param)
            val convTpe = getType(tq"${if (forAsRaw) AsRealCls else AsRawCls}[$paramTpe,${rawMethod.paramType}]")
            val c = inferCachedImplicit(convTpe, problemClue)
            val h :: t = collectParamConvs(rest :: tail)
            (c :: h) :: t
        }

        tryInferCachedImplicit(resultConvTpe)
          .map(rtc => Mapping(this, rawMethod, collectParamConvs(paramLists), rtc))
      }

      mappings match {
        case List(single) => single
        case Nil => abort(s"No raw method matches real $symbol")
        case multiple => abort(s"Multiple raw methods match real $symbol: ${multiple.map(_.rawMethod.symbol).mkString(", ")}")
      }
    }
  }

  case class Mapping(realMethod: RealMethod, rawMethod: RawMethod, paramConverters: List[List[TermName]], resultConverter: TermName) {
    def realImpl(rawName: TermName): Tree = {
      val encodedArgs = (realMethod.paramLists.flatten zip paramConverters.flatten).map {
        case (param, conv) => q"${rpcNameStr(param)} -> $conv.asRaw(${param.name.toTermName})"
      }
      val argsName = c.freshName(TermName("args"))

      q"""
        def ${realMethod.name}(...${realMethod.paramDecls}): ${realMethod.resultType} = {
          val $argsName = $MapObj[$StringCls,${rawMethod.paramType}](..$encodedArgs)
          $resultConverter.asReal($rawName.${rawMethod.name}(${realMethod.rpcName}, $argsName))
        }
       """
    }

    def rawCaseImpl(realName: TermName): CaseDef = {
      var paramIdx = 0
      val argDecls = new ListBuffer[Tree]
      val decodedArgs = (realMethod.paramLists zip paramConverters).map { case (paramList, convList) =>
        (paramList zip convList).map { case (param, conv) =>
          paramIdx += 1
          val paramName = param.name.toTermName
          val paramRpcName = rpcNameStr(param)
          val paramValue =
            if (param.asTerm.isParamWithDefault) {
              val default = q"$realName.${TermName(s"${realMethod.encodedNameStr}$$default$$$paramIdx")}"
              q"$RpcPackage.RpcUtils.getArg(args, $paramRpcName, $conv, $default)"
            }
            else
              q"$RpcPackage.RpcUtils.tryGetArg(args, $paramRpcName, $conv, rpcName)"
          argDecls += q"val $paramName = $paramValue"
          if (isRepeated(param)) q"$paramName: _*" else q"$paramName"
        }
      }
      cq"""
        ${realMethod.rpcName} =>
          ..$argDecls
          $resultConverter.asRaw($realName.${realMethod.name}(...$decodedArgs))
        """
    }
  }

  def checkImplementable(tpe: Type): Unit = {
    val sym = tpe.dealias.typeSymbol
    ensure(sym.isAbstract && sym.isClass, s"$tpe must be an abstract class or trait")
  }

  def extractRawMethods(rawTpe: Type): List[RawMethod] =
    rawTpe.members.iterator.filter(m => m.isTerm && m.isAbstract).map(RawMethod(rawTpe, _)).toList

  def extractRealMethods(realTpe: Type): List[RealMethod] =
    realTpe.members.iterator.filter(m => m.isTerm && m.isAbstract).map(RealMethod(realTpe, _)).toList

  /*
   * RPC TODO LIST:
   * - simpliest signature with args represented as map of encoded values
   * - make encoding of params/return type explicit/not necessary
   * - add method discriminator annotations
   * - add annotated-anywhere parameters
   * - param & param lists as lists, lists of lists, lists of maps, etc.
   * - RPC name for params and using default values
   * - varargs and by-name params
   * - generalization of map & list for parameters
   */
  def rpcAsReal[T: WeakTypeTag, R: WeakTypeTag]: Tree = {
    val realTpe = weakTypeOf[T]
    checkImplementable(realTpe)
    val rawTpe = weakTypeOf[R]
    checkImplementable(rawTpe)

    val selfName = c.freshName(TermName("self"))
    registerImplicit(getType(tq"$AsRealCls[$realTpe,$rawTpe]"), selfName)

    val raws = extractRawMethods(rawTpe)
    val reals = extractRealMethods(realTpe)
    val rawName = c.freshName(TermName("raw"))

    val realMethodImpls = reals.map(_.findMapping(raws, forAsRaw = false).realImpl(rawName))

    q"""
      new $AsRealCls[$realTpe,$rawTpe] { $selfName: ${TypeTree()} =>
        ..$cachedImplicitDeclarations
        def asReal($rawName: $rawTpe): $realTpe = new $realTpe {
          ..$realMethodImpls
        }
      }
    """
  }

  def rpcAsRaw[T: WeakTypeTag, R: WeakTypeTag]: Tree = {
    val realTpe = weakTypeOf[T]
    checkImplementable(realTpe)
    val rawTpe = weakTypeOf[R]
    checkImplementable(rawTpe)

    val selfName = c.freshName(TermName("self"))
    registerImplicit(getType(tq"$AsRawCls[$realTpe,$rawTpe]"), selfName)

    val raws = extractRawMethods(rawTpe)
    val reals = extractRealMethods(realTpe)
    val realName = c.freshName(TermName("real"))

    val caseDefs = raws.iterator.map(rm => (rm, new mutable.LinkedHashMap[String, CaseDef])).toMap
    reals.foreach { realMethod =>
      val mapping = realMethod.findMapping(raws, forAsRaw = true)
      val prevCaseDef = caseDefs(mapping.rawMethod).put(realMethod.rpcName, mapping.rawCaseImpl(realName))
      ensure(prevCaseDef.isEmpty,
        s"Multiple RPCs named ${realMethod.rpcName} map to raw method ${mapping.rawMethod.name}. " +
          "If you want to overload RPCs, disambiguate them with @RPCName annotation")
    }

    val rawMethodImpls = raws.map(m => m.rawImpl(caseDefs(m).values.toList))

    q"""
      new $AsRawCls[$realTpe,$rawTpe] { $selfName: ${TypeTree()} =>
        ..$cachedImplicitDeclarations
        def asRaw($realName: $realTpe): $rawTpe = new $rawTpe {
          ..$rawMethodImpls
        }
      }
     """
  }

}
