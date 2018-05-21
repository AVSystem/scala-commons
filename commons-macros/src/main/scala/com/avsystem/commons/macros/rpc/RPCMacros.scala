package com.avsystem.commons
package macros.rpc

import java.util.LinkedList

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.macros.blackbox

class RPCMacros(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  import c.universe._

  val RpcPackage = q"$CommonsPkg.rpc"
  val RpcNameType: Type = getType(tq"$RpcPackage.rpcName")
  val RpcNameNameSym: Symbol = RpcNameType.member(TermName("name"))
  val AsRealCls = tq"$RpcPackage.AsReal"
  val AsRealObj = q"$RpcPackage.AsReal"
  val AsRawCls = tq"$RpcPackage.AsRaw"
  val AsRawObj = q"$RpcPackage.AsRaw"
  val AsRealRawCls = tq"$RpcPackage.AsRealRaw"
  val AsRealRawObj = q"$RpcPackage.AsRealRaw"
  val OptionLikeCls = tq"$RpcPackage.OptionLike"
  val CanBuildFromCls = tq"$CollectionPkg.generic.CanBuildFrom"

  val RpcArityAT: Type = getType(tq"$RpcPackage.RpcArity")
  val SingleArityAT: Type = getType(tq"$RpcPackage.single")
  val OptionalArityAT: Type = getType(tq"$RpcPackage.optional")
  val MultiArityAT: Type = getType(tq"$RpcPackage.multi")
  val RpcEncodingAT: Type = getType(tq"$RpcPackage.RpcEncoding")
  val VerbatimAT: Type = getType(tq"$RpcPackage.verbatim")
  val AuxiliaryAT: Type = getType(tq"$RpcPackage.auxiliary")
  val MethodTagAT: Type = getType(tq"$RpcPackage.methodTag[_,_]")
  val ParamTagAT: Type = getType(tq"$RpcPackage.paramTag[_,_]")
  val TaggedAT: Type = getType(tq"$RpcPackage.tagged[_]")
  val RpcTagAT: Type = getType(tq"$RpcPackage.RpcTag")
  val RpcImplicitsProviderTpe: Type = getType(tq"$RpcPackage.RpcImplicitsProvider")
  val SelfAT: Type = getType(tq"$RpcPackage.self")
  val InferAT: Type = getType(tq"$RpcPackage.infer")

  val NothingTpe: Type = typeOf[Nothing]
  val StringPFTpe: Type = typeOf[PartialFunction[String, Any]]
  val BIterableTpe: Type = typeOf[Iterable[Any]]
  val BIndexedSeqTpe: Type = typeOf[IndexedSeq[Any]]

  val PartialFunctionClass: Symbol = StringPFTpe.typeSymbol
  val BIterableClass: Symbol = BIterableTpe.typeSymbol
  val BIndexedSeqClass: Symbol = BIndexedSeqTpe.typeSymbol

  sealed trait Res[+A] {
    def map[B](fun: A => B): Res[B] = this match {
      case Ok(value) => Ok(fun(value))
      case f: Failure => f
    }
    def flatMap[B](fun: A => Res[B]): Res[B] = this match {
      case Ok(value) => fun(value)
      case f: Failure => f
    }
    def toOption: Option[A] = this match {
      case Ok(value) => Some(value)
      case _ => None
    }
    def foreach(f: A => Any): Unit = this match {
      case Ok(value) => f(value)
      case _ =>
    }
  }
  case class Ok[+T](value: T) extends Res[T]
  case class Failure(message: String) extends Res[Nothing]

  sealed abstract class RpcArity(val verbatimByDefault: Boolean) {
    def encodedArgType: Type
  }
  object RpcArity {
    def fromAnnotation(annot: Tree, param: RawParam): RpcArity = {
      val at = annot.tpe
      if (at <:< SingleArityAT) RpcArity.Single(param.actualType)
      else if (at <:< OptionalArityAT) {
        val optionLikeType = typeOfCachedImplicit(param.optionLike)
        val valueMember = optionLikeType.member(TypeName("Value"))
        if (valueMember.isAbstract) {
          param.reportProblem("could not determine actual value of optional parameter type")
        }
        RpcArity.Optional(valueMember.typeSignatureIn(optionLikeType))
      }
      else if (at <:< MultiArityAT) {
        if (param.actualType <:< StringPFTpe)
          NamedMulti(param.actualType.baseType(PartialFunctionClass).typeArgs(1))
        else if (param.actualType <:< BIndexedSeqTpe)
          IndexedMulti(param.actualType.baseType(BIndexedSeqClass).typeArgs.head)
        else if (param.actualType <:< BIterableTpe)
          IterableMulti(param.actualType.baseType(BIterableClass).typeArgs.head)
        else
          param.reportProblem("@multi raw parameter must be a PartialFunction of String (for param mapping) " +
            "or Iterable (for param sequence)")
      }
      else param.reportProblem(s"unrecognized RPC arity annotation: $annot", annot.pos)
    }

    case class Single(encodedArgType: Type) extends RpcArity(true)
    case class Optional(encodedArgType: Type) extends RpcArity(true)
    case class IterableMulti(encodedArgType: Type) extends RpcArity(false)
    case class IndexedMulti(encodedArgType: Type) extends RpcArity(false)
    case class NamedMulti(encodedArgType: Type) extends RpcArity(false)
  }

  case class EncodedRealParam(realParam: RealParam, encoding: RpcEncoding) {
    def safeName: TermName = realParam.safeName
    def rawValueTree: Tree = q"${encoding.asRaw}.asRaw(${realParam.safeName})"
    def localValueDecl(body: Tree): Tree = realParam.localValueDecl(body)
  }

  sealed trait ParamMapping {
    def rawParam: RawParam
    def rawValueTree: Tree
    def realDecls: List[Tree]
  }
  object ParamMapping {
    case class Single(rawParam: RawParam, realParam: EncodedRealParam) extends ParamMapping {
      def rawValueTree: Tree =
        realParam.rawValueTree
      def realDecls: List[Tree] =
        List(realParam.localValueDecl(q"${realParam.encoding.asReal}.asReal(${rawParam.safeName})"))
    }
    case class Optional(rawParam: RawParam, wrapped: Option[EncodedRealParam]) extends ParamMapping {
      def rawValueTree: Tree =
        wrapped.fold[Tree](q"${rawParam.optionLike}.none")(erp => q"${rawParam.optionLike}.some(${erp.rawValueTree})")
      def realDecls: List[Tree] =
        wrapped.toList.map { erp =>
          val defaultValueTree = erp.realParam.defaultValueTree
          erp.realParam.localValueDecl(
            q"${rawParam.optionLike}.fold(${rawParam.safeName}, $defaultValueTree)(${erp.encoding.asReal}.asReal(_))")
        }
    }
    abstract class ListedMulti extends ParamMapping {
      protected def reals: List[EncodedRealParam]
      def rawValueTree: Tree = {
        val builderName = c.freshName(TermName("builder"))
        q"""
          val $builderName = ${rawParam.canBuildFrom}()
          $builderName.sizeHint(${reals.size})
          ..${reals.map(erp => q"$builderName += ${erp.rawValueTree}")}
          $builderName.result()
         """
      }
    }
    case class IterableMulti(rawParam: RawParam, reals: List[EncodedRealParam]) extends ListedMulti {
      def realDecls: List[Tree] = {
        val itName = c.freshName(TermName("it"))
        val itDecl = q"val $itName = ${rawParam.safeName}.iterator"
        itDecl :: reals.map { erp =>
          val rp = erp.realParam
          if (rp.symbol.asTerm.isByNameParam) {
            rp.reportProblem(
              s"${rawParam.cannotMapClue}: by-name real parameters cannot be extracted from @multi raw parameters")
          }
          erp.localValueDecl(
            q"if($itName.hasNext) ${erp.encoding.asReal}.asReal($itName.next()) else ${rp.defaultValueTree}")
        }
      }
    }
    case class IndexedMulti(rawParam: RawParam, reals: List[EncodedRealParam]) extends ListedMulti {
      def realDecls: List[Tree] = {
        reals.zipWithIndex.map { case (erp, idx) =>
          val rp = erp.realParam
          erp.realParam.localValueDecl(
            q"""
              ${rawParam.safeName}.andThen(${erp.encoding.asReal}.asReal(_))
                .applyOrElse($idx, (_: $IntCls) => ${rp.defaultValueTree})
            """)
        }
      }
    }
    case class NamedMulti(rawParam: RawParam, reals: List[EncodedRealParam]) extends ParamMapping {
      def rawValueTree: Tree = {
        val builderName = c.freshName(TermName("builder"))
        q"""
          val $builderName = ${rawParam.canBuildFrom}()
          $builderName.sizeHint(${reals.size})
          ..${reals.map(erp => q"$builderName += ((${erp.realParam.rpcName}, ${erp.rawValueTree}))")}
          $builderName.result()
         """
      }
      def realDecls: List[Tree] =
        reals.map { erp =>
          erp.realParam.localValueDecl(
            q"""
              ${rawParam.safeName}.andThen(${erp.encoding.asReal}.asReal(_))
                .applyOrElse(${erp.realParam.rpcName}, (_: $StringCls) => ${erp.realParam.defaultValueTree})
            """)
        }
    }
  }

  sealed trait RpcEncoding {
    def asRaw: Tree
    def asReal: Tree
  }
  object RpcEncoding {
    def forParam(rawParam: RawParam, realParam: RealParam): Res[RpcEncoding] = {
      val encArgType = rawParam.arity.encodedArgType
      if (rawParam.verbatim) {
        if (realParam.actualType =:= encArgType)
          Ok(Verbatim(encArgType))
        else rawParam.owner.matchFailure(
          s"${realParam.problemStr}: ${rawParam.cannotMapClue}: expected real parameter exactly of type " +
            s"$encArgType, got ${realParam.actualType}")
      } else
        Ok(RealRawEncoding(realParam.actualType, encArgType,
          Some((s"${realParam.problemStr}: ${rawParam.cannotMapClue}: ", realParam.pos))))
    }

    case class Verbatim(tpe: Type) extends RpcEncoding {
      def asRaw = q"$AsRawObj.identity[$tpe]"
      def asReal = q"$AsRealObj.identity[$tpe]"
    }
    case class RealRawEncoding(realType: Type, rawType: Type, clueWithPos: Option[(String, Position)]) extends RpcEncoding {
      private def infer(convClass: Tree): TermName = {
        val convTpe = getType(tq"$convClass[$rawType,$realType]")
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
    def pos: Position = symbol.pos
    def problemStr: String

    def reportProblem(msg: String, detailPos: Position = NoPosition): Nothing =
      abortAt(s"$problemStr: $msg", if (detailPos != NoPosition) detailPos else pos)

    val name: TermName = symbol.name.toTermName
    val safeName: TermName = c.freshName(symbol.name.toTermName)
    val nameStr: String = name.decodedName.toString
    val encodedNameStr: String = name.encodedName.toString

    def annot(tpe: Type): Option[Annot] = findAnnotation(symbol, tpe)

    override def equals(other: Any): Boolean = other match {
      case rpcSym: RpcSymbol => symbol == rpcSym.symbol
      case _ => false
    }
    override def hashCode: Int = symbol.hashCode
    override def toString: String = symbol.toString
  }

  trait RawRpcSymbol extends RpcSymbol {
    def baseTag: Type
    def defaultTag: Type

    lazy val requiredTag: Type =
      annot(TaggedAT).fold(baseTag)(_.tpe.baseType(TaggedAT.typeSymbol).typeArgs.head)

    if (!(requiredTag <:< baseTag)) {
      val msg =
        if (baseTag =:= NothingTpe)
          "cannot use @tagged, no tag annotation type specified with @methodTag/@paramTag"
        else s"tag annotation type $requiredTag specified in @tagged annotation " +
          s"must be a subtype of specified base tag $baseTag"
      reportProblem(msg)
    }

    def matchesTag(realSymbol: RealRpcSymbol): Boolean =
      realSymbol.tag(baseTag, defaultTag) <:< requiredTag
  }

  trait RealRpcSymbol extends RpcSymbol {
    def tag(baseTag: Type, defaultTag: Type): Type =
      annot(baseTag).fold(defaultTag)(_.tpe)

    lazy val rpcName: String =
      annot(RpcNameType).fold(nameStr)(_.findArg(RpcNameNameSym) match {
        case StringLiteral(n) => n
        case p => reportProblem("The name argument of @rpcName must be a string literal", p.pos)
      })
  }

  abstract class RpcTrait(val symbol: Symbol) extends RpcSymbol {
    def tpe: Type

    if (!symbol.isAbstract || !symbol.isClass) {
      reportProblem(s"it must be an abstract class or trait")
    }
  }

  abstract class RpcMethod extends RpcSymbol {
    val owner: RpcTrait
    def problemStr = s"problem with method $nameStr of type $owner"

    if (!symbol.isMethod) {
      abortAt(s"problem with member $nameStr of type $owner: it must be a method (def)", pos)
    }

    val sig: Type = symbol.typeSignatureIn(owner.tpe)
    if (sig.typeParams.nonEmpty) {
      // can we relax this?
      reportProblem("RPC methods must not be generic")
    }

    val paramLists: List[List[RpcParam]]
    val resultType: Type = sig.finalResultType

    def argLists: List[List[Tree]] = paramLists.map(_.map(_.argToPass))
    def paramDecls: List[List[Tree]] = paramLists.map(_.map(_.paramDecl))
  }

  abstract class RpcParam extends RpcSymbol {
    val owner: RpcMethod
    def problemStr = s"problem with parameter $nameStr of method ${owner.nameStr}"
    val actualType: Type = actualParamType(symbol)

    def localValueDecl(body: Tree): Tree =
      if (symbol.asTerm.isByNameParam)
        q"def $safeName = $body"
      else
        q"val $safeName = $body"

    def paramDecl: Tree = {
      val implicitFlag = if (symbol.isImplicit) Flag.IMPLICIT else NoFlags
      ValDef(Modifiers(Flag.PARAM | implicitFlag), safeName, TypeTree(symbol.typeSignature), EmptyTree)
    }

    def argToPass: Tree =
      if (isRepeated(symbol)) q"$safeName: _*" else q"$safeName"
  }

  case class RpcNameParam(owner: RawMethod, symbol: Symbol) extends RpcParam

  case class RawParam(owner: RawMethod, symbol: Symbol) extends RpcParam with RawRpcSymbol {
    def baseTag: Type = owner.baseParamTag
    def defaultTag: Type = owner.defaultParamTag

    def cannotMapClue = s"cannot map it to raw parameter $nameStr of ${owner.nameStr}"

    val arity: RpcArity =
      annot(RpcArityAT).map(a => RpcArity.fromAnnotation(a.tree, this))
        .getOrElse(RpcArity.Single(actualType))

    val verbatim: Boolean =
      annot(RpcEncodingAT).map(_.tpe <:< VerbatimAT).getOrElse(arity.verbatimByDefault)

    val auxiliary: Boolean =
      annot(AuxiliaryAT).nonEmpty

    private def infer(tpt: Tree): TermName =
      inferCachedImplicit(getType(tpt), s"$problemStr: ", pos)

    lazy val optionLike: TermName = infer(tq"$OptionLikeCls[$actualType]")

    lazy val canBuildFrom: TermName = arity match {
      case _: RpcArity.NamedMulti =>
        infer(tq"$CanBuildFromCls[$NothingCls,($StringCls,${arity.encodedArgType}),$actualType]")
      case _: RpcArity.IndexedMulti | _: RpcArity.IterableMulti =>
        infer(tq"$CanBuildFromCls[$NothingCls,${arity.encodedArgType},$actualType]")
      case _ => abort("(bug) CanBuildFrom computed for non-multi raw parameter")
    }

    def extractMapping(realParams: LinkedList[RealParam]): Res[ParamMapping] = {
      val it = realParams.listIterator()
      def maybeConsume(): Unit =
        if (!auxiliary) {
          it.remove()
        }

      def extractSingle(): Res[EncodedRealParam] =
        if (it.hasNext) {
          val realParam = it.next()
          if (matchesTag(realParam)) {
            maybeConsume()
            RpcEncoding.forParam(this, realParam).map(e => EncodedRealParam(realParam, e))
          } else extractSingle()
        } else {
          owner.matchFailure(s"raw parameter $nameStr was not matched by real parameter")
        }

      def extractOptional(): Option[EncodedRealParam] =
        if (it.hasNext) {
          val realParam = it.next()
          if (matchesTag(realParam)) {
            RpcEncoding.forParam(this, realParam).toOption.map { encoding =>
              maybeConsume()
              EncodedRealParam(realParam, encoding)
            }
          } else extractOptional()
        } else None

      def extractMulti() = {
        def loop(): Res[List[EncodedRealParam]] =
          if (it.hasNext) {
            val realParam = it.next()
            if (matchesTag(realParam)) {
              maybeConsume()
              for {
                encoding <- RpcEncoding.forParam(this, realParam)
                rest <- loop()
              } yield EncodedRealParam(realParam, encoding) :: rest
            } else loop()
          } else Ok(Nil)
        val result = loop()
        for {
          rps <- result
          (rpcName, first :: rest) <- rps.groupBy(_.realParam.rpcName) if rest.nonEmpty
        } {
          first.realParam.owner.reportProblem(
            s"multiple parameters matched to raw parameter $nameStr have the same @rpcName: $rpcName")
        }
        result
      }

      arity match {
        case _: RpcArity.Single => extractSingle().map(ParamMapping.Single(this, _))
        case _: RpcArity.Optional => Ok(ParamMapping.Optional(this, extractOptional()))
        case _: RpcArity.IterableMulti => extractMulti().map(ParamMapping.IterableMulti(this, _))
        case _: RpcArity.IndexedMulti => extractMulti().map(ParamMapping.IndexedMulti(this, _))
        case _: RpcArity.NamedMulti => extractMulti().map(ParamMapping.NamedMulti(this, _))
      }
    }
  }

  case class RealParam(owner: RealMethod, symbol: Symbol, index: Int, indexInList: Int) extends RpcParam with RealRpcSymbol {
    def defaultValueTree: Tree =
      if (symbol.asTerm.isParamWithDefault) {
        val prevListParams = owner.realParams.take(index - indexInList).map(rp => q"${rp.safeName}")
        val prevListParamss = List(prevListParams).filter(_.nonEmpty)
        q"${owner.owner.safeName}.${TermName(s"${owner.encodedNameStr}$$default$$${index + 1}")}(...$prevListParamss)"
      }
      else
        q"$RpcPackage.RpcUtils.missingArg(${owner.rpcName}, $rpcName)"
  }

  case class RawMethod(owner: RawRpcTrait, symbol: Symbol)
    extends RpcMethod with RawRpcSymbol {

    def baseTag: Type = owner.baseMethodTag
    def defaultTag: Type = owner.defaultMethodTag

    // raw method result is encoded by default, must be explicitly annotated as @verbatim to disable encoding
    val verbatimResult: Boolean =
      annot(RpcEncodingAT).exists(_.tpe <:< VerbatimAT)

    val List(baseParamTag, defaultParamTag) =
      annot(ParamTagAT)
        .map(_.tpe.baseType(ParamTagAT.typeSymbol).typeArgs)
        .getOrElse(List(owner.baseParamTag, owner.defaultParamTag))

    def matchTag(realMethod: RealMethod): Res[Unit] =
      if (matchesTag(realMethod)) Ok(())
      else {
        val tag = realMethod.tag(baseTag, defaultTag)
        matchFailure(s"it does not accept real methods tagged with $tag")
      }

    def matchFailure(msg: String): Failure =
      Failure(s"raw method $nameStr did not match because: $msg")

    val (rpcNameParam, rawParams) =
      sig.paramLists match {
        case List(nameParam) :: rest :: Nil if nameParam.typeSignature =:= typeOf[String] =>
          (RpcNameParam(this, nameParam), rest.map(RawParam(this, _)))
        case _ =>
          reportProblem("raw RPC method must take exactly two parameter lists, " +
            "the first one containing only RPC name param (String)")
      }

    val paramLists: List[List[RpcParam]] =
      List(rpcNameParam) :: rawParams :: Nil

    def rawImpl(caseDefs: List[CaseDef]): Tree =
      q"""
        def $name(...$paramDecls): $resultType =
          ${rpcNameParam.safeName} match {
            case ..$caseDefs
            case _ => $RpcPackage.RpcUtils.unknownRpc(${rpcNameParam.safeName}, $nameStr)
          }
       """
  }

  case class RealMethod(owner: RealRpcTrait, symbol: Symbol)
    extends RpcMethod with RealRpcSymbol {

    val paramLists: List[List[RealParam]] = {
      var idx = 0
      sig.paramLists.map { ss =>
        var listIdx = 0
        ss.map { s =>
          val res = RealParam(this, s, idx, listIdx)
          idx += 1
          listIdx += 1
          res
        }
      }
    }

    val realParams: List[RealParam] = paramLists.flatten
  }

  case class RawRpcTrait(tpe: Type) extends RpcTrait(tpe.typeSymbol) with RawRpcSymbol {
    def baseTag: Type = typeOf[Nothing]
    def defaultTag: Type = typeOf[Nothing]

    def problemStr: String = s"problem with raw RPC $tpe"

    val List(baseMethodTag, defaultMethodTag) =
      annot(MethodTagAT)
        .map(_.tpe.baseType(MethodTagAT.typeSymbol).typeArgs)
        .getOrElse(List(NothingTpe, NothingTpe))

    val List(baseParamTag, defaultParamTag) =
      annot(ParamTagAT)
        .map(_.tpe.baseType(ParamTagAT.typeSymbol).typeArgs)
        .getOrElse(List(NothingTpe, NothingTpe))

    lazy val rawMethods: List[RawMethod] = {
      tpe.members.iterator.filter(m => m.isTerm && m.isAbstract).map(s =>
        RawMethod(this, s)
      ).toList
    }
  }

  case class RealRpcTrait(tpe: Type)
    extends RpcTrait(tpe.typeSymbol) with RealRpcSymbol {

    def problemStr: String = s"problem with real RPC $tpe"

    lazy val realMethods: List[RealMethod] = {
      val result = tpe.members.iterator.filter(m => m.isTerm && m.isAbstract)
        .map(RealMethod(this, _)).toList
      result
    }
  }

  case class MethodMapping(realMethod: RealMethod, rawMethod: RawMethod,
    paramMappings: List[ParamMapping], resultEncoding: RpcEncoding) {

    def realImpl: Tree =
      q"""
        def ${realMethod.name}(...${realMethod.paramDecls}): ${realMethod.resultType} = {
          val ${rawMethod.rpcNameParam.safeName} = ${realMethod.rpcName}
          ..${paramMappings.map(pm => pm.rawParam.localValueDecl(pm.rawValueTree))}
          ${resultEncoding.asReal}.asReal(${rawMethod.owner.safeName}.${rawMethod.name}(...${rawMethod.argLists}))
        }
       """

    /*_*/
    // IntelliJ doesn't understand that cq returns CaseDef
    def rawCaseImpl: CaseDef =
      cq"""
        ${realMethod.rpcName} =>
          ..${paramMappings.filterNot(_.rawParam.auxiliary).flatMap(_.realDecls)}
          ${resultEncoding.asRaw}.asRaw(${realMethod.owner.safeName}.${realMethod.name}(...${realMethod.argLists}))
        """
    /*_*/
  }

  case class RpcMapping(real: RealRpcTrait, raw: RawRpcTrait, forAsReal: Boolean, forAsRaw: Boolean) {
    val selfName: TermName = c.freshName(TermName("self"))

    def setupImplicits(): Unit = {
      if(forAsReal) {
        registerImplicit(getType(tq"$AsRealCls[${raw.tpe},${real.tpe}]"), selfName)
      }
      if(forAsRaw) {
        registerImplicit(getType(tq"$AsRawCls[${raw.tpe},${real.tpe}]"), selfName)
      }
      registerCompanionImplicits(raw.tpe)
    }

    private def mappingRes(realMethod: RealMethod, rawMethod: RawMethod): Res[MethodMapping] = {
      def resultEncoding: Res[RpcEncoding] =
        if (rawMethod.verbatimResult) {
          if (rawMethod.resultType =:= realMethod.resultType)
            Ok(RpcEncoding.Verbatim(rawMethod.resultType))
          else rawMethod.matchFailure(
            s"real result type ${realMethod.resultType} does not match raw result type ${rawMethod.resultType}")
        } else {
          val e = RpcEncoding.RealRawEncoding(realMethod.resultType, rawMethod.resultType, None)
          if ((!forAsRaw || e.asRawName != termNames.EMPTY) && (!forAsReal || e.asRealName != termNames.EMPTY))
            Ok(e)
          else rawMethod.matchFailure(s"no encoding/decoding found between real result type " +
            s"${realMethod.resultType} and raw result type ${rawMethod.resultType}")
        }

      def collectParamMappings(rawParams: List[RawParam], realParams: List[RealParam]): Res[List[ParamMapping]] = {
        val realBuf = new LinkedList[RealParam]
        realBuf.addAll(realParams.asJava)

        val initialAcc: Res[List[ParamMapping]] = Ok(Nil)
        rawParams.foldLeft(initialAcc) { (accOpt, rawParam) =>
          for {
            acc <- accOpt
            mapping <- rawParam.extractMapping(realBuf)
          } yield mapping :: acc
        }.flatMap { result =>
          if (realBuf.isEmpty) Ok(result.reverse)
          else {
            val unmatched = realBuf.iterator.asScala.map(_.nameStr).mkString(",")
            rawMethod.matchFailure(s"no raw parameter(s) were found that would match real parameter(s) $unmatched")
          }
        }
      }

      for {
        _ <- rawMethod.matchTag(realMethod)
        resultConv <- resultEncoding
        paramMappings <- collectParamMappings(rawMethod.rawParams, realMethod.realParams)
      } yield MethodMapping(realMethod, rawMethod, paramMappings, resultConv)
    }

    lazy val methodMappings: List[MethodMapping] = {
      val failedReals = new ListBuffer[String]
      def addFailure(realMethod: RealMethod, message: String): Unit = {
        if (realMethod.pos != NoPosition) {
          c.error(realMethod.pos,
            s"Macro expansion at ${posInfo(c.enclosingPosition)} failed: ${realMethod.problemStr}: $message")
        } else {
          error(s"${realMethod.problemStr}: $message")
        }
        failedReals += realMethod.nameStr
      }

      val result = real.realMethods.flatMap { realMethod =>
        val methodMappings = raw.rawMethods.map(mappingRes(realMethod, _))
        methodMappings.collect { case Ok(m) => m } match {
          case List(m) => Some(m)
          case Nil =>
            val unmatchedReport = methodMappings.iterator.collect({ case Failure(error) => s" * $error" }).mkString("\n")
            addFailure(realMethod, s"it has no matching raw methods:\n$unmatchedReport")
            None
          case multiple =>
            addFailure(realMethod, s"it has multiple matching raw methods: ${multiple.map(_.rawMethod.nameStr).mkString(",")}")
            None
        }
      }

      if (failedReals.nonEmpty) {
        abort(s"Following real methods could not be mapped to raw methods: ${failedReals.mkString(",")}")
      }

      result
    }

    def asRealImpl: Tree =
      q"""
        def asReal(${raw.safeName}: ${raw.tpe}): ${real.tpe} = new ${real.tpe} {
          ..${methodMappings.map(_.realImpl)}; ()
        }
      """

    def asRawImpl: Tree = {
      val caseDefs = raw.rawMethods.iterator.map(rm => (rm, new mutable.LinkedHashMap[String, CaseDef])).toMap
      methodMappings.foreach { mapping =>
        val prevCaseDef = caseDefs(mapping.rawMethod).put(mapping.realMethod.rpcName, mapping.rawCaseImpl)
        ensure(prevCaseDef.isEmpty,
          s"Multiple RPCs named ${mapping.realMethod.rpcName} map to raw method ${mapping.rawMethod.name}. " +
            "If you want to overload RPCs, disambiguate them with @rpcName annotation")
      }

      val rawMethodImpls = raw.rawMethods.map(m => m.rawImpl(caseDefs(m).values.toList))

      q"""
        def asRaw(${real.safeName}: ${real.tpe}): ${raw.tpe} = new ${raw.tpe} {
          ..$rawMethodImpls; ()
        }
      """
    }
  }

  private def registerCompanionImplicits(rawTpe: Type): Unit =
    companionOf(rawTpe).filter { companion =>
      val typed = c.typecheck(q"$companion.implicits", silent = true)
      typed != EmptyTree && !(typed.tpe.widen =:= typeOf[Any])
    }.foreach { companion =>
      registerImplicitImport(q"import $companion.implicits._")
    }

  def rpcAsReal[R: WeakTypeTag, T: WeakTypeTag]: Tree = {
    val raw = RawRpcTrait(weakTypeOf[R].dealias)
    val real = RealRpcTrait(weakTypeOf[T].dealias)
    val mapping = RpcMapping(real, raw, forAsRaw = false, forAsReal = true)
    mapping.setupImplicits()

    // must be evaluated before `cachedImplicitDeclarations`, don't inline it into the quasiquote
    val asRealDef = mapping.asRealImpl

    q"""
      new $AsRealCls[${raw.tpe},${real.tpe}] { ${mapping.selfName}: ${TypeTree()} =>
        ..$cachedImplicitDeclarations
        $asRealDef
      }
    """
  }

  def rpcAsRaw[R: WeakTypeTag, T: WeakTypeTag]: Tree = {
    val raw = RawRpcTrait(weakTypeOf[R].dealias)
    val real = RealRpcTrait(weakTypeOf[T].dealias)
    val mapping = RpcMapping(real, raw, forAsRaw = true, forAsReal = false)
    mapping.setupImplicits()

    // must be evaluated before `cachedImplicitDeclarations`, don't inline it into the quasiquote
    val asRawDef = mapping.asRawImpl

    q"""
      new $AsRawCls[${raw.tpe},${real.tpe}] { ${mapping.selfName}: ${TypeTree()} =>
        ..$cachedImplicitDeclarations
        $asRawDef
      }
     """
  }

  def rpcAsRealRaw[R: WeakTypeTag, T: WeakTypeTag]: Tree = {
    val raw = RawRpcTrait(weakTypeOf[R].dealias)
    val real = RealRpcTrait(weakTypeOf[T].dealias)
    val mapping = RpcMapping(real, raw, forAsRaw = true, forAsReal = true)
    mapping.setupImplicits()

    // these two must be evaluated before `cachedImplicitDeclarations`, don't inline them into the quasiquote
    val asRealDef = mapping.asRealImpl
    val asRawDef = mapping.asRawImpl

    q"""
      new $AsRealRawCls[${raw.tpe},${real.tpe}] { ${mapping.selfName}: ${TypeTree()} =>
        ..$cachedImplicitDeclarations
        $asRealDef
        $asRawDef
      }
     """
  }

}
