package com.avsystem.commons
package macros.rpc

import com.avsystem.commons.macros.misc.{Fail, FailMsg, Ok, Res}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

private[commons] trait RpcMappings { this: RpcMacroCommons with RpcSymbols =>

  import c.universe._

  def collectMethodMappings[Raw <: RealMethodTarget with AritySymbol, M](
    rawSymbols: List[Raw], errorBase: String, realMethods: List[RealMethod], allowIncomplete: Boolean
  )(createMapping: (Raw, MatchedMethod) => Res[M]): List[M] = {

    val failedReals = new ListBuffer[String]
    def addFailure(realMethod: RealMethod, message: String): Unit = {
      errorAt(s"$errorBase:\n${realMethod.problemStr}:\n$message", realMethod.pos)
      failedReals += realMethod.nameStr
    }

    val indicesInRaw = new mutable.HashMap[Raw, Int]

    def matchRealAgainst(realMethod: RealMethod, rawSymbol: Raw): Res[M] = {
      val indexInRaw = indicesInRaw.getOrElse(rawSymbol, 0)
      val matchedMethod = MatchedMethod(realMethod, rawSymbol, indexInRaw, Nil)
      for {
        _ <- rawSymbol.matchTagsAndFilters(matchedMethod)
        _ <- rawSymbol.matchName(matchedMethod.real.shortDescription, matchedMethod.rawName)
        methodMapping <- createMapping(rawSymbol, matchedMethod)
      } yield {
        indicesInRaw(rawSymbol) = indexInRaw + 1
        methodMapping
      }
    }

    val result = realMethods.flatMap { realMethod =>
      Res.firstOk(rawSymbols)(matchRealAgainst(realMethod, _)) {
        case Nil =>
          s"it has illegal combination of tags (annotations)"
        case errors =>
          errors.map { case (raw, err) =>
            val unmatchedError = raw.unmatchedError.getOrElse(
              s"${raw.shortDescription.capitalize} ${raw.nameStr} did not match")
            s" * $unmatchedError:\n   ${indent(err, "   ")}"
          }.mkString("\n")
      } match {
        case Ok(v) => Some(v)
        case FailMsg(msg) =>
          if (!allowIncomplete) {
            addFailure(realMethod, msg)
          }
          None
        case Fail =>
          None
      }
    }

    result
  }

  case class EncodedRealParam(matchedParam: MatchedParam, encoding: RpcEncoding) {
    def realParam: RealParam = matchedParam.real
    def rpcName: String = matchedParam.rawName
    def safeName: TermName = matchedParam.real.safeName
    def rawValueTree: Tree = encoding.applyAsRaw(matchedParam.real.safeName)
    def localValueDecl(body: Tree): Tree = matchedParam.real.localValueDecl(body)

    private def materializeImplicit(param: Symbol): Option[Res[Tree]] =
      if (findAnnotation(param, InferAT).nonEmpty) {
        val clue = s"problem with annotation parameter ${param.name}: "
        Some(Ok(inferCachedImplicit(param.typeSignature, ErrorCtx(clue, realParam.pos)).reference(Nil)))
      } else None

    private def validatedWhenAbsentValue(annotatedDefault: Tree, expectedTpe: Type): Tree = {
      if (!(annotatedDefault.tpe <:< expectedTpe)) {
        realParam.reportProblem(s"expected value of type $expectedTpe in @whenAbsent annotation, " +
          s"got ${annotatedDefault.tpe.widen}")
      }
      val transformer = new Transformer {
        override def transform(tree: Tree): Tree = tree match {
          case Super(t@This(_), _) if !enclosingClasses.contains(t.symbol) =>
            realParam.reportProblem(s"illegal super-reference in @whenAbsent annotation")
          case This(_) if tree.symbol == realParam.owner.ownerApi.symbol => q"${realParam.owner.ownerApi.safeTermName}"
          case This(_) if !enclosingClasses.contains(tree.symbol) =>
            realParam.reportProblem(s"illegal this-reference in @whenAbsent annotation")
          case t => super.transform(t)
        }
      }
      transformer.transform(annotatedDefault)
    }

    // TODO: materialize arbitrary metadata into @whenAbsent?
    val whenAbsent: Tree =
      matchedParam.annot(WhenAbsentAT, materializeImplicit)
        .fold(EmptyTree)(annot => validatedWhenAbsentValue(annot.tree.children.tail.head, realParam.actualType))

    val rawWhenAbsent: Tree = encoding match {
      case rre: RpcEncoding.RealRawEncoding =>
        matchedParam.annot(RawWhenAbsentAT, materializeImplicit)
          .fold(EmptyTree)(annot => validatedWhenAbsentValue(annot.tree.children.tail.head, rre.decRawType))
      case _: RpcEncoding.Verbatim => EmptyTree
    }

    val hasDefaultValue: Boolean =
      whenAbsent != EmptyTree || rawWhenAbsent != EmptyTree || realParam.symbol.asTerm.isParamWithDefault

    val transientDefault: Boolean =
      hasDefaultValue && matchedParam.annot(TransientDefaultAT).nonEmpty

    def fallbackValueTree: Tree =
      if (whenAbsent != EmptyTree) c.untypecheck(whenAbsent)
      else if (rawWhenAbsent != EmptyTree) encoding.applyAsReal(c.untypecheck(rawWhenAbsent))
      else if (realParam.symbol.asTerm.isParamWithDefault) defaultValue(false)
      else q"$RpcUtils.missingArg(${matchedParam.matchedOwner.rawName}, ${matchedParam.rawName})"

    def transientValueTree: Tree =
      if (realParam.symbol.asTerm.isParamWithDefault) defaultValue(true)
      else if (whenAbsent != EmptyTree) c.untypecheck(whenAbsent)
      else encoding.applyAsReal(c.untypecheck(rawWhenAbsent))

    private def defaultValue(useThis: Boolean): Tree = {
      val prevListParams = realParam.owner.realParams
        .take(realParam.index - realParam.indexInList).map(rp => q"${rp.safeName}")
      if (realParam.encodingDependency && prevListParams.nonEmpty) {
        realParam.reportProblem("implicit dependency parameters cannot have default values " +
          "unless they are in the first parameter list - note: you can use @whenAbsent instead")
      }
      val prevListParamss = List(prevListParams).filter(_.nonEmpty)
      val realInst = if (useThis) q"this" else q"${realParam.owner.ownerApi.safeTermName}"
      q"$realInst.${TermName(s"${realParam.owner.encodedNameStr}$$default$$${realParam.index + 1}")}(...$prevListParamss)"
    }
  }
  object EncodedRealParam {
    def create(rawParam: RawValueParam, matchedParam: MatchedParam): Res[EncodedRealParam] =
      RpcEncoding.forParam(rawParam, matchedParam).map(EncodedRealParam(matchedParam, _))
  }

  sealed trait ParamMapping {
    def rawParam: RawValueParam
    def rawValueTree: Tree
    def realDecls: List[Tree]

    def cachedAsRawDecls: List[Tree]
    def cachedAsRealDecls: List[Tree]

    def allMatchedParams: List[MatchedParam] = this match {
      case ParamMapping.Single(_, erp) => List(erp.matchedParam)
      case ParamMapping.Optional(_, erpOpt) => erpOpt.map(_.matchedParam).toList
      case multi: ParamMapping.Multi => multi.reals.map(_.matchedParam)
      case ParamMapping.DummyUnit(_) => Nil
    }

    def ensureUniqueRpcNames(): Unit = this match {
      case multi: ParamMapping.Multi => multi.reals.groupBy(_.rpcName).foreach {
        case (rpcName, head :: tail) if tail.nonEmpty =>
          head.matchedParam.real.reportProblem(
            s"it has the same RPC name ($rpcName) as ${tail.size} other parameters")
        case _ =>
      }
      case _ =>
    }

  }
  object ParamMapping {
    case class Single(rawParam: RawValueParam, realParam: EncodedRealParam) extends ParamMapping {
      def rawValueTree: Tree =
        realParam.rawValueTree
      def realDecls: List[Tree] =
        List(realParam.localValueDecl(realParam.encoding.paramAsReal(rawParam.safePath, realParam.matchedParam)))

      def cachedAsRawDecls: List[Tree] = realParam.encoding.cachedAsRawDecl.toList
      def cachedAsRealDecls: List[Tree] = realParam.encoding.cachedAsRealDecl.toList
    }
    case class Optional(rawParam: RawValueParam, wrapped: Option[EncodedRealParam]) extends ParamMapping {
      def rawValueTree: Tree = {
        val noneRes: Tree = q"${rawParam.optionLike.name}.none"
        wrapped.fold(noneRes) { erp =>
          val baseRes = q"${rawParam.optionLike.name}.some(${erp.rawValueTree})"
          if (erp.transientDefault)
            q"if(${erp.safeName} != ${erp.transientValueTree}) $baseRes else $noneRes"
          else baseRes
        }
      }
      def realDecls: List[Tree] = wrapped.toList.map { erp =>
        erp.realParam.localValueDecl(erp.encoding.foldWithAsReal(
          rawParam.optionLike.name, rawParam.safePath, erp.matchedParam, erp.fallbackValueTree))
      }

      def cachedAsRawDecls: List[Tree] = wrapped.flatMap(_.encoding.cachedAsRawDecl).toList
      def cachedAsRealDecls: List[Tree] = wrapped.flatMap(_.encoding.cachedAsRealDecl).toList
    }
    abstract class Multi extends ParamMapping {
      def reals: List[EncodedRealParam]

      def cachedAsRawDecls: List[Tree] = reals.flatMap(_.encoding.cachedAsRawDecl)
      def cachedAsRealDecls: List[Tree] = reals.flatMap(_.encoding.cachedAsRealDecl)
    }
    abstract class ListedMulti extends Multi {
      def rawValueTree: Tree = rawParam.mkMulti(reals.map(_.rawValueTree))
    }
    case class IterableMulti(rawParam: RawValueParam, reals: List[EncodedRealParam]) extends ListedMulti {
      def realDecls: List[Tree] = if (reals.isEmpty) Nil else {
        val itName = c.freshName(TermName("it"))
        val itDecl = q"val $itName = ${rawParam.safePath}.iterator"
        itDecl :: reals.map { erp =>
          val rp = erp.realParam
          if (rp.symbol.asTerm.isByNameParam) {
            rp.reportProblem("by-name real parameters cannot be extracted from @multi raw parameters")
          }
          val decoded = erp.encoding.paramAsReal(q"$itName.next()", erp.matchedParam)
          erp.localValueDecl(q"if($itName.hasNext) $decoded else ${erp.fallbackValueTree}")
        }
      }
    }
    case class IndexedMulti(rawParam: RawValueParam, reals: List[EncodedRealParam]) extends ListedMulti {
      def realDecls: List[Tree] = {
        reals.zipWithIndex.map { case (erp, idx) =>
          erp.localValueDecl(
            q"""
              ${erp.encoding.andThenAsReal(rawParam.safePath, erp.matchedParam)}
                .applyOrElse($idx, (_: $IntCls) => ${erp.fallbackValueTree})
            """)
        }
      }
    }
    case class NamedMulti(rawParam: RawValueParam, reals: List[EncodedRealParam]) extends Multi {
      def rawValueTree: Tree =
        if (reals.isEmpty) q"$RpcUtils.createEmpty(${rawParam.canBuildFrom.name})" else {
          val builderName = c.freshName(TermName("builder"))
          val addStatements = reals.map { erp =>
            val baseStat = q"$builderName += ((${erp.rpcName}, ${erp.rawValueTree}))"
            if (erp.transientDefault)
              q"if(${erp.safeName} != ${erp.transientValueTree}) $baseStat"
            else baseStat
          }
          q"""
          val $builderName = $RpcUtils.createBuilder(${rawParam.canBuildFrom.name}, ${reals.size})
          ..$addStatements
          $builderName.result()
        """
        }
      def realDecls: List[Tree] =
        reals.map { erp =>
          erp.localValueDecl(
            q"""
              ${erp.encoding.andThenAsReal(rawParam.safePath, erp.matchedParam)}
                .applyOrElse(${erp.rpcName}, (_: $StringCls) => ${erp.fallbackValueTree})
            """)
        }
    }
    case class DummyUnit(rawParam: RawValueParam) extends ParamMapping {
      def rawValueTree: Tree = q"()"
      def realDecls: List[Tree] = Nil

      def cachedAsRawDecls: List[Tree] = Nil
      def cachedAsRealDecls: List[Tree] = Nil
    }
  }

  sealed trait RpcEncoding {
    protected def asRaw: Tree
    protected def asReal: Tree

    def cachedAsRawDecl: Option[Tree]
    def cachedAsRealDecl: Option[Tree]

    def applyAsRaw[T: Liftable](arg: T): Tree
    def applyAsReal[T: Liftable](arg: T): Tree
    def paramAsReal[T: Liftable](arg: T, param: MatchedParam): Tree
    def foldWithAsReal[T: Liftable](optionLike: TermName, opt: T, param: MatchedParam, fallbackValueTree: Tree): Tree
    def andThenAsReal[T: Liftable](func: T, param: MatchedParam): Tree
  }
  object RpcEncoding {
    def forParam(rawParam: RawValueParam, matchedParam: MatchedParam): Res[RpcEncoding] = {
      val realParam = matchedParam.real
      val encArgType = rawParam.arity.collectedType
      if (rawParam.verbatim) {
        if (realParam.actualType =:= encArgType)
          Ok(Verbatim(encArgType))
        else FailMsg(
          s"${realParam.problemStr}:\nexpected real parameter exactly of type " +
            s"$encArgType, got ${realParam.actualType}")
      } else {
        val errorCtx = ErrorCtx(s"${realParam.problemStr}:\n", realParam.pos)
        val implicitParams = realParam.owner.encodingDeps.takeWhile(_ != realParam)
        val encInterceptors = interceptors(encArgType, matchedParam, EncodingInterceptorTpe)
        val decInterceptors = interceptors(encArgType, matchedParam, DecodingInterceptorTpe)
        Ok(RealRawEncoding(realParam.actualType, encInterceptors, decInterceptors, encArgType,
          Some(errorCtx), realParam.tparamReferences, implicitParams))
      }
    }

    def interceptors(rawType: Type, sym: MatchedSymbol, baseIcTpe: Type): List[Annot] = {
      val baseIcSym = baseIcTpe.typeSymbol
      def filter(interceptors: List[Annot]): (List[Annot], Type) = interceptors match {
        case Nil => (Nil, rawType)
        case head :: tail =>
          val (filteredTail, tailNewRawType) = filter(tail)
          val List(newRawType, oldRawType) = head.tpe.baseType(baseIcSym).typeArgs
          if (oldRawType =:= tailNewRawType)
            (head :: filteredTail, newRawType)
          else
            (filteredTail, tailNewRawType)
      }
      val (res, _) = filter(sym.annots(baseIcTpe))
      res
    }

    case class Verbatim(tpe: Type) extends RpcEncoding {
      protected def asRaw: Tree = q"$AsRawObj.identity[$tpe]"
      protected def asReal: Tree = q"$AsRealObj.identity[$tpe]"

      def cachedAsRawDecl: Option[Tree] = None
      def cachedAsRealDecl: Option[Tree] = None

      def applyAsRaw[T: Liftable](arg: T): Tree = q"$arg"
      def applyAsReal[T: Liftable](arg: T): Tree = q"$arg"
      def paramAsReal[T: Liftable](arg: T, param: MatchedParam): Tree = q"$arg"
      def foldWithAsReal[T: Liftable](optionLike: TermName, opt: T, param: MatchedParam, fallbackValueTree: Tree): Tree =
        q"$optionLike.getOrElse($opt, $fallbackValueTree)"
      def andThenAsReal[T: Liftable](func: T, param: MatchedParam): Tree = q"$func"
    }

    case class RealRawEncoding(
      realType: Type,
      encInterceptors: List[Annot],
      decInterceptors: List[Annot],
      rawType: Type,
      errorCtx: Option[ErrorCtx],
      typeParams: List[RealTypeParam],
      implicitParams: List[RealParam]
    ) extends RpcEncoding {

      val decRawType: Type = decInterceptors.headOption
        .map(_.tpe.baseType(DecodingInterceptorTpe.typeSymbol).typeArgs.head)
        .getOrElse(rawType)

      private def infer(encNotDec: Boolean): Tree = {
        val convClass = if (encNotDec) AsRawCls else AsRealCls
        val interceptors = if (encNotDec) encInterceptors else decInterceptors
        val interceptorBase = if (encNotDec) EncodingInterceptorTpe else DecodingInterceptorTpe
        val finalRawType =
          interceptors.headOption.map(_.tpe.baseType(interceptorBase.typeSymbol).typeArgs.head).getOrElse(rawType)
        val convTpe = getType(tq"$convClass[$finalRawType,$realType]")
        val tparams = typeParams.map(_.symbol)
        val implicitDeps = implicitParams.map(_.actualType)
        val res = errorCtx match {
          case Some(ctx) =>
            inferCachedImplicit(convTpe, ctx, tparams, implicitDeps)
              .reference(implicitParams.map(_.argToPass))
          case None =>
            tryInferCachedImplicit(convTpe, tparams, implicitDeps)
              .map(_.reference(implicitParams.map(_.argToPass))).getOrElse(EmptyTree)
        }
        // TODO: avoid wrapping and unnecessary instantiation of AsRaw/AsReal, cache interceptors
        val interceptMethod = TermName(if (encNotDec) "interceptEnc" else "interceptDec")
        if (res == EmptyTree) res
        else interceptors.foldLeft(res) {
          (acc, interceptor) => q"$RpcUtils.$interceptMethod($acc, ${c.untypecheck(interceptor.tree)})"
        }
      }

      private lazy val asRawTree: Tree = infer(true)
      private lazy val asRealTree: Tree = infer(false)

      protected def asRaw: Tree = if (encInterceptors.nonEmpty) q"$cachedAsRawName" else asRawTree
      protected def asReal: Tree = if (decInterceptors.nonEmpty) q"$cachedAsRealName" else asRealTree

      private val cachedAsRawName = c.freshName(TermName("asRaw"))
      private val cachedAsRealName = c.freshName(TermName("asReal"))

      def cachedAsRawDecl: Option[Tree] =
        if (encInterceptors.nonEmpty) Some(q"val $cachedAsRawName = $asRawTree") else None
      def cachedAsRealDecl: Option[Tree] =
        if (encInterceptors.nonEmpty) Some(q"val $cachedAsRealName = $asRealTree") else None

      def hasAsRaw: Boolean = asRawTree != EmptyTree
      def hasAsReal: Boolean = asRealTree != EmptyTree

      def applyAsRaw[T: Liftable](arg: T): Tree =
        q"$asRaw.asRaw($arg)"
      def applyAsReal[T: Liftable](arg: T): Tree =
        q"$asReal.asReal($arg)"
      def paramAsReal[T: Liftable](arg: T, param: MatchedParam): Tree =
        q"$RpcUtils.readArg(${param.matchedOwner.rawName}, ${param.rawName}, $asReal, $arg)"
      def foldWithAsReal[T: Liftable](optionLike: TermName, opt: T, param: MatchedParam, fallbackValueTree: Tree): Tree =
        q"$optionLike.fold($opt, $fallbackValueTree)(raw => ${paramAsReal(q"raw", param)})"
      def andThenAsReal[T: Liftable](func: T, param: MatchedParam): Tree =
        q"$func.andThen(raw => ${paramAsReal(q"raw", param)})"
    }
  }

  case class MethodMapping(
    matchedMethod: MatchedMethod,
    rawMethod: RawMethod,
    paramMappingList: List[ParamMapping],
    resultEncoding: RpcEncoding
  ) {

    def realMethod: RealMethod = matchedMethod.real
    def rpcName: String = matchedMethod.rawName

    val paramMappings: Map[RawValueParam, ParamMapping] =
      paramMappingList.iterator.map(m => (m.rawParam, m)).toMap

    def ensureUniqueRpcNames(): Unit =
      paramMappings.valuesIterator.filterNot(_.rawParam.auxiliary).toList
        .flatMap(_.allMatchedParams).groupBy(_.rawName)
        .foreach {
          case (rpcName, head :: tail) if tail.nonEmpty =>
            head.real.reportProblem(s"it has the same RPC name ($rpcName) as ${tail.size} other parameters")
          case _ =>
        }

    private def rawValueTree(rawParam: RawParam): Tree = rawParam match {
      case _: MethodNameParam => q"$rpcName"
      case rvp: RawValueParam => paramMappings(rvp).rawValueTree
      case crp: CompositeRawParam =>
        q"""
          ..${crp.paramLists.flatten.map(p => p.localValueDecl(rawValueTree(p)))}
          new ${crp.actualType}(...${crp.paramLists.map(_.map(_.safeName))})
         """
    }

    private def maybeTry(tree: Tree): Tree =
      if (rawMethod.tried) q"$TryObj($tree)" else tree

    private def maybeUntry(tree: Tree): Tree =
      if (rawMethod.tried) q"$tree.get" else tree

    def realImpl: Tree = stripTparamRefs(realMethod.sig.typeParams) {
      val rawCall = q"${rawMethod.ownerTrait.safeTermName}.${rawMethod.name}(...${rawMethod.argLists})"
      q"""
        def ${realMethod.name}[..${realMethod.typeParamDecls}](...${realMethod.paramDecls}): ${realMethod.resultType} = {
          ..${paramMappings.values.flatMap(_.cachedAsRawDecls)}
          ..${rawMethod.rawParams.map(rp => rp.localValueDecl(rawValueTree(rp)))}
          ..${resultEncoding.cachedAsRealDecl}
          ${maybeUntry(resultEncoding.applyAsReal(rawCall))}
        }
       """
    }

    def rawCaseImpl: Tree = {
      val realCall = q"${realMethod.ownerApi.safeTermName}.${realMethod.name}(...${realMethod.argLists})"
      val caseImpl =
        q"""
          ..${paramMappings.values.flatMap(_.cachedAsRealDecls)}
          ..${paramMappings.values.filterNot(_.rawParam.auxiliary).flatMap(_.realDecls)}
          ..${resultEncoding.cachedAsRawDecl}
          ${resultEncoding.applyAsRaw(maybeTry(realCall))}
        """

      if (realMethod.typeParams.isEmpty) caseImpl
      else stripTparamRefs(realMethod.sig.typeParams) {
        val fakeMethodName = c.freshName(TermName("gendef"))
        q"def $fakeMethodName[..${realMethod.typeParams.map(_.typeParamDecl)}] = $caseImpl; $fakeMethodName"
      }
    }
  }

  case class RpcMapping(real: RealRpcApi, raw: RawRpcTrait, forAsReal: Boolean, forAsRaw: Boolean) {
    val selfName: TermName = c.freshName(TermName("self"))

    if (forAsReal) {
      registerImplicit(getType(tq"$AsRealCls[${raw.tpe},${real.tpe}]"), selfName)
    }
    if (forAsRaw) {
      registerImplicit(getType(tq"$AsRawCls[${raw.tpe},${real.tpe}]"), selfName)
    }

    private def extractMapping(
      method: MatchedMethod, rawParam: RawValueParam, parser: ParamsParser[RealParam]
    ): Res[ParamMapping] = {
      def matchedParam(real: RealParam, indexInRaw: Int): Option[MatchedParam] =
        rawParam.matchRealParam(method, real, indexInRaw).toOption

      def createErp(real: RealParam, indexInRaw: Int): Option[Res[EncodedRealParam]] =
        matchedParam(real, indexInRaw).map(EncodedRealParam.create(rawParam, _))

      val consume = !rawParam.auxiliary
      rawParam.arity match {
        case _: ParamArity.Single =>
          val unmatchedErrorMsg = rawParam.unmatchedError.getOrElse(
            s"${raw.shortDescription} ${rawParam.pathStr} was not matched by real parameter")
          parser.extractSingle(consume, createErp(_, 0), unmatchedErrorMsg).map(ParamMapping.Single(rawParam, _))
        case _: ParamArity.Optional =>
          Ok(ParamMapping.Optional(rawParam, parser.extractOptional(consume, createErp(_, 0))))
        case ParamArity.Multi(_, true) =>
          parser.extractMulti(consume, createErp).map(ParamMapping.NamedMulti(rawParam, _))
        case _: ParamArity.Multi if rawParam.actualType <:< BIndexedSeqTpe =>
          parser.extractMulti(consume, createErp).map(ParamMapping.IndexedMulti(rawParam, _))
        case _: ParamArity.Multi =>
          parser.extractMulti(consume, createErp).map(ParamMapping.IterableMulti(rawParam, _))
      }
    }

    private def mappingRes(rawMethod: RawMethod, matchedMethod: MatchedMethod): Res[MethodMapping] = {
      val realMethod = matchedMethod.real
      val realResultType =
        if (rawMethod.tried) getType(tq"$TryCls[${realMethod.resultType}]") else realMethod.resultType

      def resultEncoding: Res[RpcEncoding] =
        if (rawMethod.verbatimResult) {
          if (rawMethod.resultType =:= realResultType)
            Ok(RpcEncoding.Verbatim(rawMethod.resultType))
          else
            FailMsg(s"real result type $realResultType does not match raw result type ${rawMethod.resultType}")
        } else {
          val encInterceptors = RpcEncoding.interceptors(rawMethod.resultType, matchedMethod, EncodingInterceptorTpe)
          val decInterceptors = RpcEncoding.interceptors(rawMethod.resultType, matchedMethod, DecodingInterceptorTpe)
          val enc = RpcEncoding.RealRawEncoding(realResultType, encInterceptors, decInterceptors, rawMethod.resultType,
            None, realMethod.resultTparamReferences, realMethod.encodingDeps)
          if ((!forAsRaw || enc.hasAsRaw) && (!forAsReal || enc.hasAsReal))
            Ok(enc)
          else {
            val failedConv = if (forAsRaw) AsRawCls else AsRealCls
            FailMsg(implicitNotFoundMsg(getType(tq"$failedConv[${rawMethod.resultType},$realResultType]")))
          }
        }

      for {
        resultConv <- resultEncoding
        realParams = matchedMethod.real.encodingDeps ++ matchedMethod.real.regularParams
        paramMappings <- collectParamMappings(realParams, rawMethod.allValueParams, allowIncomplete = false)(
          extractMapping(matchedMethod, _, _),
          rp => rawMethod.errorForUnmatchedParam(rp).getOrElse(
            s"no raw parameter was found that would match ${rp.shortDescription} ${rp.nameStr}")
        )
      } yield MethodMapping(matchedMethod, rawMethod, paramMappings, resultConv)
    }

    lazy val methodMappings: List[MethodMapping] = {
      val errorBase = raw.unmatchedError.getOrElse(s"cannot translate between $real and $raw")
      collectMethodMappings(raw.rawMethods, errorBase, real.realMethods, allowIncomplete = false)(mappingRes)
    }

    def ensureUniqueRpcNames(): Unit =
      methodMappings.groupBy(_.matchedMethod.rawName).foreach {
        case (_, single :: Nil) =>
          single.ensureUniqueRpcNames()
        case (rpcName, head :: tail) => head.realMethod.reportProblem(
          s"it has the same RPC name ($rpcName) as ${tail.size} other methods - " +
            s"if you want to overload RPC methods, disambiguate them with @rpcName")
        case _ =>
      }

    def asRealImpl: Tree =
      q"""
        def asReal(${raw.safeTermName}: ${raw.tpe}): ${real.tpe} = new ${real.tpe} {
          ..${methodMappings.map(_.realImpl)}; ()
        }
      """

    def asRawImpl: Tree = {
      val caseImpls = raw.rawMethods.iterator.map(rm => (rm, new mutable.LinkedHashMap[String, Tree])).toMap
      methodMappings.foreach { mapping =>
        caseImpls(mapping.rawMethod).put(mapping.rpcName, mapping.rawCaseImpl)
      }
      val rawMethodImpls = raw.rawMethods.map(m => m.rawImpl(caseImpls(m).toList))

      q"""
        def asRaw(${real.safeTermName}: ${real.tpe}): ${raw.tpe} = new ${raw.tpe} {
          ..$rawMethodImpls; ()
        }
      """
    }
  }
}
