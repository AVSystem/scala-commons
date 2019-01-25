package com.avsystem.commons
package macros.rpc

import com.avsystem.commons.macros.misc.{Fail, Ok, Res}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

private[commons] trait RpcMappings { this: RpcMacroCommons with RpcSymbols =>

  import c.universe._

  def collectMethodMappings[Raw <: TagMatchingSymbol with AritySymbol, M](
    rawSymbols: List[Raw], errorBase: String, realMethods: List[RealMethod], allowIncomplete: Boolean)(
    createMapping: (Raw, MatchedMethod) => Res[M]): List[M] = {

    val failedReals = new ListBuffer[String]
    def addFailure(realMethod: RealMethod, message: String): Unit = {
      errorAt(s"${realMethod.problemStr}: $message", realMethod.pos)
      failedReals += realMethod.nameStr
    }

    val result = realMethods.flatMap { realMethod =>
      Res.firstOk(rawSymbols)(rawSymbol => for {
        fallbackTag <- rawSymbol.matchTag(realMethod)
        matchedMethod = MatchedMethod(realMethod, fallbackTag)
        _ <- rawSymbol.matchName(matchedMethod.real.shortDescription, matchedMethod.rawName)
        _ <- rawSymbol.matchFilters(matchedMethod)
        methodMapping <- createMapping(rawSymbol, matchedMethod)
      } yield methodMapping) { errors =>
        val unmatchedReport = errors.map { case (raw, err) =>
          s" * ${raw.unmatchedError}: ${indent(err, " ")}"
        }.mkString("\n")
        s"$errorBase:\n$unmatchedReport"
      } match {
        case Ok(v) => Some(v)
        case Fail(msg) =>
          if (!allowIncomplete) {
            addFailure(realMethod, msg)
          }
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
  }
  object EncodedRealParam {
    def create(rawParam: RawValueParam, matchedParam: MatchedParam): Res[EncodedRealParam] =
      RpcEncoding.forParam(rawParam, matchedParam.real).map(EncodedRealParam(matchedParam, _))
  }

  sealed trait ParamMapping {
    def rawParam: RawValueParam
    def rawValueTree: Tree
    def realDecls: List[Tree]

    def allMatchedParams: List[MatchedParam] = this match {
      case ParamMapping.Single(_, erp) => List(erp.matchedParam)
      case ParamMapping.Optional(_, erpOpt) => erpOpt.map(_.matchedParam).toList
      case multi: ParamMapping.Multi => multi.reals.map(_.matchedParam)
      case ParamMapping.DummyUnit(_) => Nil
    }
  }
  object ParamMapping {
    case class Single(rawParam: RawValueParam, realParam: EncodedRealParam) extends ParamMapping {
      def rawValueTree: Tree =
        realParam.rawValueTree
      def realDecls: List[Tree] =
        List(realParam.localValueDecl(realParam.encoding.paramAsReal(rawParam.safePath, realParam.matchedParam)))
    }
    case class Optional(rawParam: RawValueParam, wrapped: Option[EncodedRealParam]) extends ParamMapping {
      def rawValueTree: Tree = {
        val noneRes: Tree = q"${rawParam.optionLike}.none"
        wrapped.fold(noneRes) { erp =>
          val baseRes = q"${rawParam.optionLike}.some(${erp.rawValueTree})"
          if (erp.matchedParam.transientDefault)
            q"if(${erp.safeName} != ${erp.matchedParam.transientValueTree}) $baseRes else $noneRes"
          else baseRes
        }
      }
      def realDecls: List[Tree] = wrapped.toList.map { erp =>
        erp.realParam.localValueDecl(erp.encoding.foldWithAsReal(
          rawParam.optionLike, rawParam.safePath, erp.matchedParam))
      }
    }
    abstract class Multi extends ParamMapping {
      def reals: List[EncodedRealParam]
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
            rp.reportProblem(
              s"${rawParam.cannotMapClue}: by-name real parameters cannot be extracted from @multi raw parameters")
          }
          val decoded = erp.encoding.paramAsReal(q"$itName.next()", erp.matchedParam)
          erp.localValueDecl(q"if($itName.hasNext) $decoded else ${erp.matchedParam.fallbackValueTree}")
        }
      }
    }
    case class IndexedMulti(rawParam: RawValueParam, reals: List[EncodedRealParam]) extends ListedMulti {
      def realDecls: List[Tree] = {
        reals.zipWithIndex.map { case (erp, idx) =>
          erp.localValueDecl(
            q"""
              ${erp.encoding.andThenAsReal(rawParam.safePath, erp.matchedParam)}
                .applyOrElse($idx, (_: $IntCls) => ${erp.matchedParam.fallbackValueTree})
            """)
        }
      }
    }
    case class NamedMulti(rawParam: RawValueParam, reals: List[EncodedRealParam]) extends Multi {
      def rawValueTree: Tree =
        if (reals.isEmpty) q"$RpcUtils.createEmpty(${rawParam.canBuildFrom})" else {
          val builderName = c.freshName(TermName("builder"))
          val addStatements = reals.map { erp =>
            val baseStat = q"$builderName += ((${erp.rpcName}, ${erp.rawValueTree}))"
            if (erp.matchedParam.transientDefault)
              q"if(${erp.safeName} != ${erp.matchedParam.transientValueTree}) $baseStat"
            else baseStat
          }
          q"""
          val $builderName = $RpcUtils.createBuilder(${rawParam.canBuildFrom}, ${reals.size})
          ..$addStatements
          $builderName.result()
        """
        }
      def realDecls: List[Tree] =
        reals.map { erp =>
          erp.localValueDecl(
            q"""
              ${erp.encoding.andThenAsReal(rawParam.safePath, erp.matchedParam)}
                .applyOrElse(${erp.rpcName}, (_: $StringCls) => ${erp.matchedParam.fallbackValueTree})
            """)
        }
    }
    case class DummyUnit(rawParam: RawValueParam) extends ParamMapping {
      def rawValueTree: Tree = q"()"
      def realDecls: List[Tree] = Nil
    }
  }

  sealed trait RpcEncoding {
    def asRaw: Tree
    def asReal: Tree

    def applyAsRaw[T: Liftable](arg: T): Tree
    def applyAsReal[T: Liftable](arg: T): Tree
    def paramAsReal[T: Liftable](arg: T, param: MatchedParam): Tree
    def foldWithAsReal[T: Liftable](optionLike: TermName, opt: T, param: MatchedParam): Tree
    def andThenAsReal[T: Liftable](func: T, param: MatchedParam): Tree
  }
  object RpcEncoding {
    def forParam(rawParam: RawValueParam, realParam: RealParam): Res[RpcEncoding] = {
      val encArgType = rawParam.arity.collectedType
      if (rawParam.verbatim) {
        if (realParam.actualType =:= encArgType)
          Ok(Verbatim(encArgType))
        else Fail(
          s"${realParam.problemStr}: ${rawParam.cannotMapClue}: expected real parameter exactly of type " +
            s"$encArgType, got ${realParam.actualType}")
      } else
        Ok(RealRawEncoding(realParam.actualType, encArgType,
          Some((s"${realParam.problemStr}: ${rawParam.cannotMapClue}: ", realParam.pos))))
    }

    case class Verbatim(tpe: Type) extends RpcEncoding {
      def asRaw: Tree = q"$AsRawObj.identity[$tpe]"
      def asReal: Tree = q"$AsRealObj.identity[$tpe]"
      def applyAsRaw[T: Liftable](arg: T): Tree = q"$arg"
      def applyAsReal[T: Liftable](arg: T): Tree = q"$arg"
      def paramAsReal[T: Liftable](arg: T, param: MatchedParam): Tree = q"$arg"
      def foldWithAsReal[T: Liftable](optionLike: TermName, opt: T, param: MatchedParam): Tree =
        q"$optionLike.getOrElse($opt, ${param.fallbackValueTree})"
      def andThenAsReal[T: Liftable](func: T, param: MatchedParam): Tree = q"$func"
    }
    case class RealRawEncoding(realType: Type, rawType: Type, clueWithPos: Option[(String, Position)])
      extends RpcEncoding {

      private def infer(convClass: Tree): TermName = {
        val convTpe = getType(tq"$convClass[$rawType,$realType]")
        clueWithPos match {
          case Some((clue, pos)) => inferCachedImplicit(convTpe, clue, pos)
          case None => tryInferCachedImplicit(convTpe).getOrElse(termNames.EMPTY)
        }
      }
      lazy val asRawName: TermName = infer(AsRawCls)
      lazy val asRealName: TermName = infer(AsRealCls)
      def asRaw: Tree = q"$asRawName"
      def asReal: Tree = q"$asRealName"

      def applyAsRaw[T: Liftable](arg: T): Tree =
        q"$asRaw.asRaw($arg)"
      def applyAsReal[T: Liftable](arg: T): Tree =
        q"$asReal.asReal($arg)"
      def paramAsReal[T: Liftable](arg: T, param: MatchedParam): Tree =
        q"$RpcUtils.readArg(${param.matchedOwner.rawName}, ${param.rawName}, $asReal, $arg)"
      def foldWithAsReal[T: Liftable](optionLike: TermName, opt: T, param: MatchedParam): Tree =
        q"$optionLike.fold($opt, ${param.fallbackValueTree})(raw => ${paramAsReal(q"raw", param)})"
      def andThenAsReal[T: Liftable](func: T, param: MatchedParam): Tree =
        q"$func.andThen(raw => ${paramAsReal(q"raw", param)})"
    }
  }

  case class MethodMapping(matchedMethod: MatchedMethod, rawMethod: RawMethod,
    paramMappingList: List[ParamMapping], resultEncoding: RpcEncoding) {

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

    def realImpl: Tree =
      q"""
        def ${realMethod.name}(...${realMethod.paramDecls}): ${realMethod.resultType} = {
          ..${rawMethod.rawParams.map(rp => rp.localValueDecl(rawValueTree(rp)))}
          ${maybeUntry(resultEncoding.applyAsReal(q"${rawMethod.owner.safeName}.${rawMethod.name}(...${rawMethod.argLists})"))}
        }
       """

    def rawCaseImpl: Tree =
      q"""
        ..${paramMappings.values.filterNot(_.rawParam.auxiliary).flatMap(_.realDecls)}
        ${resultEncoding.applyAsRaw(maybeTry(q"${realMethod.owner.safeName}.${realMethod.name}(...${realMethod.argLists})"))}
      """
  }

  case class RpcMapping(real: RealRpcTrait, raw: RawRpcTrait, forAsReal: Boolean, forAsRaw: Boolean) {
    val selfName: TermName = c.freshName(TermName("self"))

    if (forAsReal) {
      registerImplicit(getType(tq"$AsRealCls[${raw.tpe},${real.tpe}]"), selfName)
    }
    if (forAsRaw) {
      registerImplicit(getType(tq"$AsRawCls[${raw.tpe},${real.tpe}]"), selfName)
    }

    private def extractMapping(method: MatchedMethod, rawParam: RawValueParam, parser: ParamsParser[RealParam]): Res[ParamMapping] = {
      def matchedParam(real: RealParam, indexInRaw: Int): Option[MatchedParam] =
        rawParam.matchRealParam(method, real, indexInRaw).toOption

      def createErp(real: RealParam, indexInRaw: Int): Option[Res[EncodedRealParam]] =
        matchedParam(real, indexInRaw).map(EncodedRealParam.create(rawParam, _))

      val consume = !rawParam.auxiliary
      rawParam.arity match {
        case _: ParamArity.Single =>
          val unmatchedError = s"${raw.shortDescription} ${rawParam.pathStr} was not matched by real parameter"
          parser.extractSingle(consume, createErp(_, 0), unmatchedError).map(ParamMapping.Single(rawParam, _))
        case _: ParamArity.Optional =>
          Ok(ParamMapping.Optional(rawParam, parser.extractOptional(consume, createErp(_, 0))))
        case ParamArity.Multi(_, true) =>
          parser.extractMulti(consume, createErp).map(ParamMapping.NamedMulti(rawParam, _))
        case _: ParamArity.Multi if rawParam.actualType <:< BIndexedSeqTpe =>
          parser.extractMulti(consume, createErp).map(ParamMapping.IndexedMulti(rawParam, _))
        case _: ParamArity.Multi =>
          parser.extractMulti(consume, createErp).map(ParamMapping.IterableMulti(rawParam, _))
        case ParamArity.Fail(error) =>
          parser.findFirst(matchedParam(_, 0))
            .map(m => Fail(s"${m.real.problemStr}: $error"))
            .getOrElse(Ok(ParamMapping.DummyUnit(rawParam)))
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
            Fail(s"real result type $realResultType does not match raw result type ${rawMethod.resultType}")
        } else {
          val e = RpcEncoding.RealRawEncoding(realResultType, rawMethod.resultType, None)
          if ((!forAsRaw || e.asRawName != termNames.EMPTY) && (!forAsReal || e.asRealName != termNames.EMPTY))
            Ok(e)
          else {
            val failedConv = if (forAsRaw) AsRawCls else AsRealCls
            Fail(implicitNotFoundMsg(getType(tq"$failedConv[${rawMethod.resultType},$realResultType]")))
          }
        }

      for {
        resultConv <- resultEncoding
        paramMappings <- collectParamMappings(
          matchedMethod.real.realParams, rawMethod.allValueParams, "raw parameter", allowIncomplete = false
        )(extractMapping(matchedMethod, _, _))
      } yield MethodMapping(matchedMethod, rawMethod, paramMappings, resultConv)
    }

    lazy val methodMappings: List[MethodMapping] = {
      val errorBase = s"it has no matching raw methods in ${raw.description}"
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
        def asReal(${raw.safeName}: ${raw.tpe}): ${real.tpe} = new ${real.tpe} {
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
        def asRaw(${real.safeName}: ${real.tpe}): ${raw.tpe} = new ${raw.tpe} {
          ..$rawMethodImpls; ()
        }
      """
    }
  }
}
