package com.avsystem.commons
package macros.rpc

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.reflect.macros.blackbox

class RPCMacros(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  import c.universe._

  val RpcPackage = q"$CommonsPackage.rpc"
  val RPCNameType: Type = getType(tq"$RpcPackage.RPCName")
  val AsRealCls = tq"$RpcPackage.AsReal"
  val AsRawCls = tq"$RpcPackage.AsRaw"

  abstract class RpcMethod {
    val owner: Type
    val symbol: Symbol
    val name: TermName = symbol.name.toTermName

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
  }

  case class RealMethod(owner: Type, symbol: Symbol) extends RpcMethod {
    val rpcName: String = allAnnotations(symbol).find(_.tree.tpe <:< RPCNameType).map { annot =>
      annot.tree.children.tail match {
        case List(StringLiteral(str)) => str
        case _ => c.abort(annot.tree.pos, "The argument of @RPCName must be a string literal.")
      }
    }.getOrElse(symbol.name.decodedName.toString)

    val paramLists: List[List[Symbol]] = sig.paramLists

    def paramDecls: List[List[ValDef]] = paramLists.map(_.map { ps =>
      val implicitFlag = if (ps.isImplicit) Flag.IMPLICIT else NoFlags
      ValDef(Modifiers(Flag.PARAM | implicitFlag), ps.name.toTermName, TypeTree(ps.typeSignature), EmptyTree)
    })

    def findMapping(rawMethods: List[RawMethod]): Mapping = {
      val mappings = rawMethods.flatMap { rawMethod =>
        val resultConvTpe = getType(tq"$AsRealCls[$resultType,${rawMethod.resultType}]")
        val returnTypeConv = inferCachedImplicit(resultConvTpe)

        def collectParamConvs(params: List[List[Symbol]]): Option[List[List[TermName]]] = params match {
          case Nil => Some(Nil)
          case Nil :: tail => collectParamConvs(tail).map(Nil :: _)
          case (param :: rest) :: tail =>
            val convTpe = getType(tq"$AsRawCls[${param.typeSignature},${rawMethod.paramType}]")
            for {
              c <- inferCachedImplicit(convTpe)
              h :: t <- collectParamConvs(rest :: tail)
            } yield (c :: h) :: t
        }
        for {
          rtc <- returnTypeConv
          pc <- collectParamConvs(paramLists)
        } yield Mapping(this, rawMethod, pc, rtc)
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
      val encodedParams = (realMethod.paramLists.flatten zip paramConverters.flatten).map {
        case (param, conv) => q"${param.name.decodedName.toString} -> $conv.asRaw(${param.name.toTermName})"
      }

      q"""
        def ${realMethod.name}(...${realMethod.paramDecls}): ${realMethod.resultType} =
          $resultConverter.asReal($rawName.${rawMethod.name}(${realMethod.rpcName}, $MapObj(..$encodedParams)))

       """
    }
  }

  def checkImplementable(tpe: Type): Unit = {
    val sym = tpe.dealias.typeSymbol
    ensure(sym.isAbstract && sym.isClass, s"$tpe must be an abstract class or trait")
  }

  def extractRawMethods(rawTpe: Type): List[RawMethod] =
    rawTpe.members.iterator.filter(m => m.isTerm && m.isAbstract).map(RawMethod(rawTpe, _)).toList

  def extractRealMethods(realTpe: Type): List[RealMethod] = {
    val res = realTpe.members.iterator.filter(m => m.isTerm && m.isAbstract).map(RealMethod(realTpe, _)).toList
    res.groupBy(_.rpcName).foreach {
      case (_, List(_)) =>
      case (name, _) =>
        abort(s"Multiple real methods with the same RPC name $name found in $realTpe")
    }
    res
  }

  /*
   * RPC TODO LIST:
   * - simpliest signature with args represented as map of encoded values
   * - make encoding of params/return type explicit/not necessary
   * - add method discriminator annotations
   * - add annotated-anywhere parameters
   * - param & param lists as lists, lists of lists, lists of maps, etc.
   * - RPC name for params and using default values
   */
  def rpcAsReal[T: WeakTypeTag, R: WeakTypeTag]: Tree = {
    val realTpe = weakTypeOf[T]
    checkImplementable(realTpe)
    val rawTpe = weakTypeOf[R]
    checkImplementable(rawTpe)

    val raws = extractRawMethods(rawTpe)
    val reals = extractRealMethods(realTpe)
    val rawName = c.freshName(TermName("raw"))

    val realMethodImpls = reals.map(_.findMapping(raws).realImpl(rawName))
    val res =
      q"""
       new $AsRealCls[$realTpe,$rawTpe] {
         ..$cachedImplicitDeclarations
         def asReal($rawName: $rawTpe): $realTpe = new $realTpe {
           ..$realMethodImpls
         }
       }
     """

    //    error(show(res))
    res
  }

}
