package com.avsystem.commons
package macros.rpc

import java.util.{LinkedList => JLinkedList}

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.reflect.macros.blackbox

class RPCMacros(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  import c.universe._

  val RpcPackage = q"$CommonsPackage.rpc"
  val RPCNameType: Type = getType(tq"$RpcPackage.RPCName")
  val RPCNameNameSym: Symbol = RPCNameType.member(TermName("name"))
  val AsRealCls = tq"$RpcPackage.AsReal"
  val AsRealObj = q"$RpcPackage.AsReal"
  val AsRawCls = tq"$RpcPackage.AsRaw"
  val AsRawObj = q"$RpcPackage.AsRaw"
  val OptionLikeCls = tq"$RpcPackage.OptionLike"
  val CanBuildFromCls = tq"$CollectionPkg.generic.CanBuildFrom"

  val RpcArityAT: Type = getType(tq"$RpcPackage.RpcArity")
  val SingleArityAT: Type = getType(tq"$RpcPackage.single")
  val OptionalArityAT: Type = getType(tq"$RpcPackage.optional")
  val RepeatedArityAT: Type = getType(tq"$RpcPackage.repeated")
  val NamedRepeatedArityAT: Type = getType(tq"$RpcPackage.namedRepeated")
  val RpcEncodingAT: Type = getType(tq"$RpcPackage.RpcEncoding")
  val EncodedAT: Type = getType(tq"$RpcPackage.encoded")
  val RpcFilterAT: Type = getType(tq"$RpcPackage.RpcFilter")
  val AnnotatedWithAT: Type = getType(tq"$RpcPackage.annotatedWith[_]")

  val BIterableClass: ClassSymbol = rootMirror.staticClass("scala.collection.Iterable")
  val PartialFunctionClass: ClassSymbol = rootMirror.staticClass("scala.PartialFunction")

  sealed abstract class RpcArity
  object RpcArity {
    def fromAnnotation(annot: Tree): RpcArity = {
      val at = annot.tpe
      if (at <:< SingleArityAT) RpcArity.Single
      else if (at <:< OptionalArityAT) RpcArity.Optional
      else if (at <:< RepeatedArityAT) RpcArity.Repeated
      else if (at <:< NamedRepeatedArityAT) RpcArity.NamedRepeated
      else abortAt("Unrecognized RPC arity annotation", annot.pos)
    }

    case object Single extends RpcArity
    case object Optional extends RpcArity
    case object Repeated extends RpcArity
    case object NamedRepeated extends RpcArity
  }

  case class EncodedRealParam(realParam: RealParam, encoding: RpcEncoding) {
    def safeName: TermName = realParam.safeName
    def rawValueTree: Tree = q"${encoding.asRaw}.asRaw(${realParam.safeName})"
  }

  sealed trait ParamMapping {
    def rawParam: RawParam
    def rawValueTree: Tree
    def realDecls(nameOfRealRpc: TermName): List[Tree]
  }
  object ParamMapping {
    case class Single(rawParam: RawParam, realParam: EncodedRealParam) extends ParamMapping {
      def rawValueTree: Tree =
        realParam.rawValueTree
      def realDecls(nameOfRealRpc: TermName): List[Tree] =
        List(q"val ${realParam.safeName} = ${realParam.encoding.asReal}.asReal(${rawParam.safeName})")
    }
    case class Optional(rawParam: RawParam, wrapped: Option[EncodedRealParam]) extends ParamMapping {
      def rawValueTree: Tree =
        wrapped.fold[Tree](q"${rawParam.optionLike}.none")(erp => q"${rawParam.optionLike}.some(${erp.rawValueTree})")
      def realDecls(nameOfRealRpc: TermName): List[Tree] =
        wrapped.toList.map { erp =>
          val defaultValueTree = erp.realParam.defaultValueTree(nameOfRealRpc)
          q"""
            val ${erp.realParam.safeName} = ${rawParam.optionLike}.fold(
              ${rawParam.safeName}, $defaultValueTree)(${erp.encoding.asReal}.asReal)
            """
        }
    }
    case class Repeated(rawParam: RawParam, reals: List[EncodedRealParam]) extends ParamMapping {
      def rawValueTree: Tree = {
        val builderName = c.freshName(TermName("builder"))
        q"""
          val $builderName = ${rawParam.canBuildFrom}()
          ..${reals.map(erp => q"$builderName += ${erp.rawValueTree}")}
          $builderName.result()
         """
      }
      def realDecls(nameOfRealRpc: TermName): List[Tree] = {
        val itName = c.freshName(TermName("it"))
        val itDecl = q"val $itName = ${rawParam.safeName}.iterator"
        itDecl :: reals.map { erp =>
          val defaultValueTree = erp.realParam.defaultValueTree(nameOfRealRpc)
          q"""
            val ${erp.realParam.safeName} =
              if($itName.hasNext) ${erp.encoding.asReal}.asReal($itName.next()) else $defaultValueTree
            """
        }
      }
    }
    case class NamedRepeated(rawParam: RawParam, reals: List[EncodedRealParam]) extends ParamMapping {
      def rawValueTree: Tree = {
        val builderName = c.freshName(TermName("builder"))
        q"""
          val $builderName = ${rawParam.canBuildFrom}()
          ..${reals.map(erp => q"$builderName += ((${erp.realParam.rpcName}, ${erp.rawValueTree}))")}
          $builderName.result()
         """
      }
      def realDecls(nameOfRealRpc: TermName): List[Tree] =
        reals.map { erp =>
          val defaultValueTree = erp.realParam.defaultValueTree(nameOfRealRpc)
          q"""
            val ${erp.realParam.safeName} = ${rawParam.safeName}.andThen(${erp.encoding.asReal}.asReal)
              .applyOrElse(${erp.realParam.rpcName}, (_: $StringCls) => $defaultValueTree)
            """
        }
    }
  }

  sealed trait RpcFilter {
    def matches(sym: RpcSymbol): Boolean
  }
  object RpcFilter {
    def fromAnnotation(annot: Tree): RpcFilter = {
      val tpe = annot.tpe
      if (tpe <:< AnnotatedWithAT) RpcFilter.AnnotatedWith(tpe.dealias.typeArgs.head)
      else abortAt("Unrecognized RPC filter annotation", annot.pos)
    }

    case class AnnotatedWith(annotTpe: Type) extends RpcFilter {
      def matches(sym: RpcSymbol): Boolean = sym.annotations.exists(_.tpe <:< annotTpe)
    }
  }

  sealed trait RpcEncoding {
    def asRaw: Tree
    def asReal: Tree
  }
  object RpcEncoding {
    def forParam(rawParam: RawParam, realParam: RealParam): RpcEncoding = {
      val problemClue = s"Problem with parameter ${realParam.nameStr} of RPC ${realParam.owner.nameStr}: "
      val realParamPos = realParam.symbol.pos
      if (rawParam.encoded) {
        if (realParam.symbol.asTerm.isByNameParam) {
          abortAt(s"${problemClue}encoded RPC parameters cannot be passed by name", realParamPos)
        }
        RealRawEncoding(realParam.actualType, rawParam.encodedArgType, Some((problemClue, realParamPos)))
      } else {
        if (!(realParam.actualType =:= rawParam.encodedArgType)) {
          abortAt(s"${problemClue}expected RPC parameter exactly of type ${rawParam.encodedArgType}", realParamPos)
        }
        Verbatim(rawParam.encodedArgType)
      }
    }

    case class Verbatim(tpe: Type) extends RpcEncoding {
      def asRaw = q"$AsRawObj.identity[$tpe]"
      def asReal = q"$AsRealObj.identity[$tpe]"
    }
    case class RealRawEncoding(realType: Type, rawType: Type, clueWithPos: Option[(String, Position)]) extends RpcEncoding {
      private def infer(convClass: Tree): TermName = {
        val convTpe = getType(tq"$convClass[$realType,$rawType]")
        clueWithPos match {
          case Some((clue, pos)) => inferCachedImplicit(convTpe, clue, pos)
          case None => tryInferCachedImplicit(convTpe).getOrElse(termNames.EMPTY)
        }
      }
      lazy val asRawName: TermName = infer(AsRawCls)
      lazy val asRealName: TermName = infer(AsRealCls)
      def asRaw = q"$asRawName"
      def asReal = q"$asRealName"
    }
  }

  abstract class RpcSymbol {
    val symbol: Symbol
    val annotations: List[Tree] = allAnnotations(symbol)

    val name: TermName = symbol.name.toTermName
    val safeName: TermName = RPCMacros.this.safeName(symbol)
    val nameStr: String = name.decodedName.toString
    val encodedNameStr: String = name.encodedName.toString

    lazy val filters: List[RpcFilter] = annotations.collect {
      case filterAnnot if filterAnnot.tpe <:< RpcFilterAT => RpcFilter.fromAnnotation(filterAnnot)
    }

    lazy val rpcName: String =
      annotations.find(_.tpe <:< RPCNameType)
        .map { annot =>
          findAnnotationArg(annot, RPCNameNameSym) match {
            case StringLiteral(n) => n
            case p => abortAt("The `name` argument of @RPCName must be a string literal.", p.pos)
          }
        }.getOrElse(nameStr)

    override def equals(other: Any): Boolean = other match {
      case rpcSym: RpcSymbol => symbol == rpcSym.symbol
      case _ => false
    }
    override def hashCode: Int = symbol.hashCode
    override def toString: String = symbol.toString
  }

  abstract class RpcMethod extends RpcSymbol {
    val owner: Type

    if (!symbol.isMethod) {
      abortAt(s"$nameStr of $owner is not a method", symbol.pos)
    }

    val sig: Type = symbol.typeSignatureIn(owner)
    if (sig.typeParams.nonEmpty) {
      // can we relax this?
      abortAt(s"$nameStr of $owner has type parameters", symbol.pos)
    }

    val paramLists: List[List[RpcParam]]
    val resultType: Type = sig.finalResultType

    def argLists: List[List[Tree]] = paramLists.map(_.map(_.argToPass))
    def paramDecls: List[List[Tree]] = paramLists.map(_.map(_.paramDecl))
  }

  abstract class RpcParam extends RpcSymbol {
    val owner: RpcMethod
    val actualType: Type = actualParamType(symbol)

    def paramDecl: Tree = {
      val implicitFlag = if (symbol.isImplicit) Flag.IMPLICIT else NoFlags
      ValDef(Modifiers(Flag.PARAM | implicitFlag), safeName, TypeTree(symbol.typeSignature), EmptyTree)
    }

    def argToPass: Tree =
      if (isRepeated(symbol)) q"$safeName: _*" else q"$safeName"
  }

  case class RpcNameParam(owner: RawMethod, symbol: Symbol) extends RpcParam

  case class RawParam(owner: RawMethod, symbol: Symbol) extends RpcParam {
    val arity: RpcArity = annotations.find(_.tpe <:< RpcArityAT)
      .map(RpcArity.fromAnnotation).getOrElse(RpcArity.Single)

    private def infer(tpt: Tree): TermName = inferCachedImplicit(
      getType(tpt),
      s"Problem with raw parameter $nameStr of ${owner.nameStr}",
      symbol.pos
    )

    lazy val optionLike: TermName = infer(tq"$OptionLikeCls[$actualType]")

    lazy val encodedArgType: Type = arity match {
      case RpcArity.Single => actualType
      case RpcArity.Optional =>
        val optionLikeType = typeOfCachedImplicit(optionLike)
        val valueMember = optionLikeType.member(TypeName("Value"))
        if (valueMember.isAbstract) {
          abortAt(s"Could not determine actual value of optional raw parameter $nameStr", symbol.pos)
        }
        valueMember.typeSignatureIn(optionLikeType)
      case RpcArity.Repeated =>
        if (actualType <:< typeOf[scala.collection.Iterable[Any]])
          actualType.baseType(BIterableClass).typeArgs.head
        else
          abortAt(s"Raw parameter $nameStr corresponding to repeated real parameters must be an Iterable", symbol.pos)
      case RpcArity.NamedRepeated =>
        if (actualType <:< typeOf[PartialFunction[String, Any]])
          actualType.baseType(PartialFunctionClass).typeArgs(1)
        else
          abortAt(s"Raw parameter $nameStr corresponding to repeated name real parameters must be a PartialFunction on String", symbol.pos)
    }

    lazy val canBuildFrom: TermName = arity match {
      case RpcArity.Repeated =>
        infer(tq"$CanBuildFromCls[Nothing,$encodedArgType,$actualType]")
      case RpcArity.NamedRepeated =>
        infer(tq"$CanBuildFromCls[Nothing,($StringCls,$encodedArgType),$actualType]")
      case _ => termNames.EMPTY
    }

    val encoded: Boolean =
      annotations.find(_.tpe <:< RpcEncodingAT).exists(_.tpe <:< EncodedAT)

    def filtersMatch(realMethod: RealParam): Boolean =
      filters.forall(_.matches(realMethod))

    def extractMapping(realParams: JLinkedList[RealParam]): Option[ParamMapping] = {
      val it = realParams.listIterator()

      def extractSingle(): Option[EncodedRealParam] = if (it.hasNext) {
        val realParam = it.next()
        if (filtersMatch(realParam)) {
          it.remove()
          Some(EncodedRealParam(realParam, RpcEncoding.forParam(this, realParam)))
        } else extractSingle()
      } else None

      def extractOptional(): Option[EncodedRealParam] = if (it.hasNext) {
        val realParam = it.next()
        if (filtersMatch(realParam)) {
          if (encoded || encodedArgType =:= realParam.actualType) {
            it.remove()
            Some(EncodedRealParam(realParam, RpcEncoding.forParam(this, realParam)))
          } else None
        } else extractOptional()
      } else None

      def extractRepeated(): List[EncodedRealParam] = if (it.hasNext) {
        val realParam = it.next()
        if (filtersMatch(realParam)) {
          it.remove()
          val erp = EncodedRealParam(realParam, RpcEncoding.forParam(this, realParam))
          erp :: extractRepeated()
        } else extractRepeated()
      } else Nil

      arity match {
        case RpcArity.Single => extractSingle().map(ParamMapping.Single(this, _))
        case RpcArity.Optional => Some(ParamMapping.Optional(this, extractOptional()))
        case RpcArity.Repeated => Some(ParamMapping.Repeated(this, extractRepeated()))
        case RpcArity.NamedRepeated => Some(ParamMapping.NamedRepeated(this, extractRepeated()))
      }
    }
  }

  case class RealParam(owner: RealMethod, symbol: Symbol, index: Int) extends RpcParam {
    def defaultValueTree(nameOfRealRpc: TermName): Tree =
      if (symbol.asTerm.isParamWithDefault)
        q"$nameOfRealRpc.${TermName(s"${owner.encodedNameStr}$$default$$$index")}"
      else
        q"$RpcPackage.RpcUtils.missingArg(${owner.rpcName}, $rpcName)"
  }

  case class RawMethod(owner: Type, symbol: Symbol) extends RpcMethod {
    val resultEncoded: Boolean =
      annotations.find(_.tpe <:< RpcEncodingAT).exists(_.tpe <:< EncodedAT)

    def filtersMatch(realMethod: RealMethod): Boolean =
      filters.forall(_.matches(realMethod))

    val (rpcNameParam, rawParams, paramLists) = {
      def failSigMsg = s"$nameStr of $owner has wrong signature: it must take RPC name (a String) as first parameter"
      sig.paramLists match {
        case (nameParam :: tailFirst) :: rest if nameParam.typeSignature =:= typeOf[String] =>
          val np = RpcNameParam(this, nameParam)
          val tailFirstRaw = tailFirst.map(RawParam(this, _))
          val restRaw = rest.map(_.map(RawParam(this, _)))
          val rp: List[RawParam] = tailFirstRaw ::: restRaw.flatten
          val pl: List[List[RpcParam]] = (np :: tailFirstRaw) :: restRaw
          (np, rp, pl)
        case _ =>
          abortAt(failSigMsg, symbol.pos)
      }
    }

    def rawImpl(caseDefs: List[CaseDef]): Tree =
      q"""
        def $name(...$paramDecls): $resultType =
          ${rpcNameParam.safeName} match {
            case ..$caseDefs
            case _ => $RpcPackage.RpcUtils.unknownRpc(${rpcNameParam.safeName}, $nameStr)
          }
       """
  }

  case class RealMethod(owner: Type, symbol: Symbol) extends RpcMethod {
    val paramLists: List[List[RealParam]] = {
      var idx = 0
      def nextIdx() = {
        idx += 1
        idx
      }
      sig.paramLists.map(_.map(s => RealParam(this, s, nextIdx())))
    }

    val realParams: List[RealParam] = paramLists.flatten

    realParams.groupBy(_.rpcName).foreach {
      case (_, List(_)) =>
      case (n, _) => abortAt(s"Multiple parameters of RPC $nameStr have the same @RPCName $n", symbol.pos)
    }

    def findMapping(rawMethods: List[RawMethod], forAsRaw: Boolean): MethodMapping = {
      val methodMappings = rawMethods.filter(_.filtersMatch(this)).flatMap { rawMethod =>
        def resultEncoding: Option[RpcEncoding] =
          if (rawMethod.resultEncoded)
            Some(RpcEncoding.RealRawEncoding(resultType, rawMethod.resultType, None)).filter { e =>
              val requiredConverter = if (forAsRaw) e.asRawName else e.asRealName
              requiredConverter != termNames.EMPTY
            }
          else if (resultType =:= rawMethod.resultType) Some(RpcEncoding.Verbatim(rawMethod.resultType))
          else None

        def collectParamMappings(rawParams: List[RawParam], realParams: List[RealParam]): Option[List[ParamMapping]] = {
          val realBuf = new JLinkedList[RealParam]
          realBuf.addAll(realParams.asJava)

          val initialAcc = Option(List.empty[ParamMapping])
          rawParams.foldLeft(initialAcc) { (accOpt, rawParam) =>
            for {
              acc <- accOpt
              mapping <- rawParam.extractMapping(realBuf)
            } yield mapping :: acc
          }.filter(_ => realBuf.isEmpty).map(_.reverse)
        }

        for {
          resultConv <- resultEncoding
          paramMappings <- collectParamMappings(rawMethod.rawParams, realParams)
        } yield MethodMapping(this, rawMethod, paramMappings, resultConv)
      }

      methodMappings match {
        case List(single) => single
        case Nil => abortAt(s"No raw method matches real $symbol", symbol.pos)
        case multiple => abort(s"Multiple raw methods match real $symbol: ${multiple.map(_.rawMethod).mkString(", ")}")
      }
    }
  }

  case class MethodMapping(realMethod: RealMethod, rawMethod: RawMethod,
    paramMappings: List[ParamMapping], resultEncoding: RpcEncoding) {

    def realImpl(rawName: TermName): Tree = {
      val rawParamDefns = paramMappings.map { pm =>
        q"val ${pm.rawParam.safeName} = ${pm.rawValueTree}"
      }

      q"""
        def ${realMethod.name}(...${realMethod.paramDecls}): ${realMethod.resultType} = {
          val ${rawMethod.rpcNameParam.safeName} = ${realMethod.rpcName}
          ..$rawParamDefns
          ${resultEncoding.asReal}.asReal($rawName.${rawMethod.name}(...${rawMethod.argLists}))
        }
       """
    }

    def rawCaseImpl(nameOfRealRpc: TermName): CaseDef = {
      cq"""
        ${realMethod.rpcName} =>
          ..${paramMappings.flatMap(_.realDecls(nameOfRealRpc))}
          ${resultEncoding.asRaw}.asRaw($nameOfRealRpc.${realMethod.name}(...${realMethod.argLists}))
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
