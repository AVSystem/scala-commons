package com.avsystem.commons
package macros.rpc

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.collection.mutable
import scala.reflect.macros.blackbox

class RPCMacros(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  import c.universe._

  val RpcPackage = q"$CommonsPackage.rpc"
  val RPCNameType: Type = getType(tq"$RpcPackage.RPCName")
  val RPCNameNameSym: Symbol = RPCNameType.member(TermName("name"))
  val AsRealCls = tq"$RpcPackage.AsReal"
  val AsRawCls = tq"$RpcPackage.AsRaw"
  val AnnotatedWithAnnotTpe: Type = getType(tq"$RpcPackage.annotatedWith[_]")

  def rpcNameStr(sym: Symbol): String = allAnnotations(sym)
    .find(_.tpe <:< RPCNameType)
    .map { annot =>
      findAnnotationArg(annot, RPCNameNameSym) match {
        case StringLiteral(name) => name
        case p => abortAt("The `name` argument of @RPCName must be a string literal.", p.pos)
      }
    }.getOrElse(sym.nameStr)

  abstract class RpcMethod {
    val owner: Type
    val symbol: Symbol
    val name: TermName = symbol.name.toTermName
    val nameStr: String = name.decodedName.toString
    val encodedNameStr: String = name.encodedName.toString

    if (!symbol.isMethod) {
      abortAt(s"$nameStr of $owner is not a method", symbol.pos)
    }

    val sig: Type = symbol.typeSignatureIn(owner)
    if (sig.typeParams.nonEmpty) {
      // can we relax this?
      abortAt(s"$nameStr of $owner has type parameters", symbol.pos)
    }

    val paramLists: List[List[Symbol]] = sig.paramLists
    val resultType: Type = sig.finalResultType

    def argLists: List[List[Tree]] =
      paramLists.map(_.map { ps =>
        val res = q"${ps.safeName}"
        if (isRepeated(ps)) q"$res: _*" else res
      })

    def paramDecls: List[List[Tree]] =
      paramLists.map(_.map { ps =>
        val implicitFlag = if (ps.isImplicit) Flag.IMPLICIT else NoFlags
        ValDef(Modifiers(Flag.PARAM | implicitFlag), ps.safeName, TypeTree(ps.typeSignature), EmptyTree)
      })
  }

  case class RawMethod(owner: Type, symbol: Symbol) extends RpcMethod {
    val (rpcNameParam, encodedParams) = {
      def failSigMsg = s"$nameStr of $owner has wrong signature: it must take RPC name as first parameter " +
        "and maps of encoded real arguments as rest of parameters"
      sig.paramLists match {
        case (nameParam :: tailFirst) :: rest if nameParam.typeSignature =:= typeOf[String] =>
          val encParams = (tailFirst :: rest).flatten
          encParams.foreach { argsParam =>
            val argsTpe = argsParam.typeSignature.dealias
            val validSig = argsTpe.typeSymbol == MapSym && argsTpe.typeArgs.head =:= typeOf[String]
            if (!validSig) {
              abortAt(failSigMsg, symbol.pos)
            }
          }
          (nameParam, encParams)
        case _ =>
          abortAt(failSigMsg, symbol.pos)
      }
    }

    def rawImpl(caseDefs: List[CaseDef]): Tree =
      q"""
        def $name(...$paramDecls): $resultType =
          ${rpcNameParam.safeName} match {
            case ..$caseDefs
            case rpcName => $RpcPackage.RpcUtils.unknownRpc(rpcName, $nameStr)
          }
       """
  }

  case class RealMethod(owner: Type, symbol: Symbol) extends RpcMethod {
    val rpcName: String = rpcNameStr(symbol)

    paramLists.flatten.groupBy(rpcNameStr).foreach {
      case (_, List(_)) =>
      case (n, _) => abortAt(s"Multiple parameters of RPC $nameStr have the same @RPCName $n", symbol.pos)
    }

    def findMapping(rawMethods: List[RawMethod], forAsRaw: Boolean): MethodMapping = {
      val methodMappings = rawMethods.flatMap { rawMethod =>
        val resultConvTpe = getType(tq"${if (forAsRaw) AsRawCls else AsRealCls}[$resultType,${rawMethod.resultType}]")

        def inferConverter(param: Symbol, encodedType: Type): TermName = {
          val paramNameStr = param.name.decodedName.toString
          val problemClue = s"Problem with parameter $paramNameStr of RPC $nameStr: "
          if (param.asTerm.isByNameParam) {
            abortAt(s"${problemClue}encoded RPC parameters cannot be passed by name", param.pos)
          }
          val paramTpe = actualParamType(param)
          val convTpe = getType(tq"${if (forAsRaw) AsRealCls else AsRawCls}[$paramTpe,$encodedType]")
          inferCachedImplicit(convTpe, problemClue, param.pos)
        }

        def collectParamMappings(rawParams: List[Symbol], realParams: List[Symbol]): Option[List[(Symbol, ParamMapping)]] =
          (rawParams, realParams) match {
            case (Nil, Nil) => Some(Nil)
            case (Nil, _) => None
            case (rawParam :: rest, _) =>
              val reqAnnotTypes = allAnnotations(rawParam).collect {
                case annot if annot.tpe <:< AnnotatedWithAnnotTpe =>
                  annot.tpe.dealias.typeArgs.head
              }

              val (matchingReals, remainingReals) =
                if (reqAnnotTypes.nonEmpty)
                  realParams.partition { realParam =>
                    val realAnnots = allAnnotations(realParam)
                    reqAnnotTypes.forall(rat => realAnnots.exists(_.tpe <:< rat))
                  }
                else (realParams, Nil)

              collectParamMappings(rest, remainingReals).map { result =>
                // V from Map[String,V]
                val encodedType = rawParam.typeSignature.dealias.typeArgs(1)
                val converters = matchingReals.map(param => (param, inferConverter(param, encodedType)))
                (rawParam, ParamMapping(converters, encodedType)) :: result
              }
          }

        for {
          resultConv <- tryInferCachedImplicit(resultConvTpe)
          paramMappings <- collectParamMappings(rawMethod.encodedParams, paramLists.flatten)
        } yield MethodMapping(this, rawMethod, paramMappings, resultConv)
      }

      methodMappings match {
        case List(single) => single
        case Nil => abortAt(s"No raw method matches real $symbol", symbol.pos)
        case multiple => abort(s"Multiple raw methods match real $symbol: ${multiple.map(_.rawMethod.symbol).mkString(", ")}")
      }
    }
  }

  case class ParamMapping(converters: List[(Symbol, TermName)], encodedType: Type)

  case class MethodMapping(realMethod: RealMethod, rawMethod: RawMethod,
    paramMappings: List[(Symbol, ParamMapping)], resultConverter: TermName) {

    def realImpl(rawName: TermName): Tree = {
      val rawParamDefns = paramMappings.map {
        case (rawParam, ParamMapping(converters, encodedType)) =>
          val encoded = converters.map { case (param, conv) =>
            q"${rpcNameStr(param)} -> $conv.asRaw(${param.safeName})"
          }
          q"val ${rawParam.safeName} = $MapObj[$StringCls,$encodedType](..$encoded)"
      }

      q"""
        def ${realMethod.name}(...${realMethod.paramDecls}): ${realMethod.resultType} = {
          val ${rawMethod.rpcNameParam.safeName} = ${realMethod.rpcName}
          ..$rawParamDefns
          $resultConverter.asReal($rawName.${rawMethod.name}(...${rawMethod.argLists}))
        }
       """
    }

    def rawCaseImpl(realName: TermName): CaseDef = {
      val argDecls = paramMappings.flatMap {
        case (rawParam, ParamMapping(converters, _)) =>
          converters.map { case (realParam, conv) =>
            val paramRpcName = rpcNameStr(realParam)
            val paramValue =
              if (realParam.asTerm.isParamWithDefault) {
                val default = q"$realName.${TermName(s"${realMethod.encodedNameStr}$$default$$${paramIndex(realParam)}")}"
                q"$RpcPackage.RpcUtils.getArg(${rawParam.safeName}, $paramRpcName, $conv, $default)"
              }
              else
                q"$RpcPackage.RpcUtils.tryGetArg(${rawParam.safeName}, $paramRpcName, $conv, ${realMethod.rpcName})"
            q"val ${realParam.safeName} = $paramValue"
          }
      }

      cq"""
        ${realMethod.rpcName} =>
          ..$argDecls
          $resultConverter.asRaw($realName.${realMethod.name}(...${realMethod.argLists}))
        """
    }
  }

  def checkImplementable(tpe: Type): Unit = {
    val sym = tpe.dealias.typeSymbol
    if (!sym.isAbstract || !sym.isClass) {
      abortAt(s"$sym must be an abstract class or trait", sym.pos)
    }
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
