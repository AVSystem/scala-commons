package com.avsystem.commons
package macros.rpc

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait RpcMappings { this: RpcMacroCommons with RpcSymbols =>

  import c.universe._

  def collectMethodMappings[R <: RawRpcSymbol with AritySymbol, M](
    rawSymbols: List[R], rawShortDesc: String, realMethods: List[RealMethod])(
    createMapping: (R, MatchedMethod) => Res[M]): List[M] = {

    val failedReals = new ListBuffer[String]
    def addFailure(realMethod: RealMethod, message: String): Unit = {
      errorAt(s"${realMethod.problemStr}: $message", realMethod.pos)
      failedReals += realMethod.nameStr
    }

    val result = realMethods.flatMap { realMethod =>
      Res.firstOk(rawSymbols)(rawSymbol => for {
        fallbackTag <- rawSymbol.matchTag(realMethod)
        matchedMethod = MatchedMethod(realMethod, fallbackTag)
        _ <- rawSymbol.matchName(matchedMethod)
        _ <- rawSymbol.matchFilters(matchedMethod)
        methodMapping <- createMapping(rawSymbol, matchedMethod)
      } yield methodMapping) { errors =>
        val unmatchedReport = errors.map { case (raw, err) =>
          s" * ${raw.shortDescription} ${raw.nameStr} did not match: $err"
        }.mkString("\n")
        s"it has no matching $rawShortDesc:\n$unmatchedReport"
      } match {
        case Ok(v) => Some(v)
        case Fail(msg) =>
          addFailure(realMethod, msg)
          None
      }
    }

    if (failedReals.nonEmpty) {
      abort(s"Following real methods could not be mapped to $rawShortDesc: ${failedReals.mkString(",")}")
    }

    result
  }

  def collectParamMappings[R <: RealParamTarget, M](raws: List[R], rawShortDesc: String, matchedMethod: MatchedMethod)
    (createMapping: (R, ParamsParser) => Res[M]): Res[List[M]] = {

    val parser = new ParamsParser(matchedMethod)
    Res.traverse(raws)(createMapping(_, parser)).flatMap { result =>
      if (parser.remaining.isEmpty) Ok(result)
      else {
        val unmatched = parser.remaining.iterator.map(_.nameStr).mkString(",")
        Fail(s"no $rawShortDesc(s) were found that would match real parameter(s) $unmatched")
      }
    }
  }

  class ParamsParser(matchedMethod: MatchedMethod) {

    import scala.collection.JavaConverters._

    private val realParams = new java.util.LinkedList[RealParam]
    realParams.addAll(matchedMethod.real.realParams.asJava)

    def remaining: Seq[RealParam] = realParams.asScala

    def extractSingle[B](raw: RealParamTarget, matcher: MatchedParam => Res[B]): Res[B] = {
      val it = realParams.listIterator()
      def loop(): Res[B] =
        if (it.hasNext) {
          val real = it.next()
          raw.matchRealParam(matchedMethod, real) match {
            case Ok(matchedParam) =>
              if (!raw.auxiliary) {
                it.remove()
              }
              matcher(matchedParam)
            case Fail(_) => loop()
          }
        } else Fail(s"${raw.shortDescription} ${raw.pathStr} was not matched by real parameter")
      loop()
    }

    def extractOptional[B](raw: RealParamTarget, matcher: MatchedParam => Res[B]): Option[B] = {
      val it = realParams.listIterator()
      def loop(): Option[B] =
        if (it.hasNext) {
          val real = it.next()
          raw.matchRealParam(matchedMethod, real) match {
            case Ok(matchedParam) =>
              val res = matcher(matchedParam).toOption
              if (!raw.auxiliary) {
                res.foreach(_ => it.remove())
              }
              res
            case Fail(_) => loop()
          }
        } else None
      loop()
    }

    def extractMulti[B](raw: RealParamTarget, matcher: (MatchedParam, Int) => Res[B], named: Boolean): Res[List[B]] = {
      val it = realParams.listIterator()
      def loop(result: ListBuffer[B]): Res[List[B]] =
        if (it.hasNext) {
          val real = it.next()
          raw.matchRealParam(matchedMethod, real) match {
            case Ok(matchedParam) =>
              if (!raw.auxiliary) {
                it.remove()
              }
              matcher(matchedParam, result.size) match {
                case Ok(b) =>
                  result += b
                  loop(result)
                case fail: Fail =>
                  fail
              }
            case Fail(_) => loop(result)
          }
        } else Ok(result.result())
      loop(new ListBuffer[B])
    }
  }

  case class EncodedRealParam(matchedParam: MatchedParam, encoding: RpcEncoding) {
    def realParam: RealParam = matchedParam.real
    def rpcName: String = matchedParam.rpcName
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
    }
  }
  object ParamMapping {
    case class Single(rawParam: RawValueParam, realParam: EncodedRealParam) extends ParamMapping {
      def rawValueTree: Tree =
        realParam.rawValueTree
      def realDecls: List[Tree] =
        List(realParam.localValueDecl(realParam.encoding.applyAsReal(rawParam.safePath)))
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
        val defaultValueTree = erp.matchedParam.fallbackValueTree
        erp.realParam.localValueDecl(erp.encoding.foldWithAsReal(
          rawParam.optionLike, rawParam.safePath, defaultValueTree))
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
          erp.localValueDecl(
            q"if($itName.hasNext) ${erp.encoding.applyAsReal(q"$itName.next()")} else ${erp.matchedParam.fallbackValueTree}")
        }
      }
    }
    case class IndexedMulti(rawParam: RawValueParam, reals: List[EncodedRealParam]) extends ListedMulti {
      def realDecls: List[Tree] = {
        reals.zipWithIndex.map { case (erp, idx) =>
          erp.localValueDecl(
            q"""
              ${erp.encoding.andThenAsReal(rawParam.safePath)}
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
              ${erp.encoding.andThenAsReal(rawParam.safePath)}
                .applyOrElse(${erp.rpcName}, (_: $StringCls) => ${erp.matchedParam.fallbackValueTree})
            """)
        }
    }
  }

  sealed trait RpcEncoding {
    def asRaw: Tree
    def asReal: Tree

    def applyAsRaw[T: Liftable](arg: T): Tree = q"$asRaw.asRaw($arg)"
    def applyAsReal[T: Liftable](arg: T): Tree = q"$asReal.asReal($arg)"
    def foldWithAsReal[T: Liftable](optionLike: TermName, opt: T, default: Tree): Tree =
      q"$optionLike.fold($opt, $default)($asReal.asReal(_))"
    def andThenAsReal[T: Liftable](func: T): Tree = q"$func.andThen($asReal.asReal(_))"
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
      def asRaw = q"$AsRawObj.identity[$tpe]"
      def asReal = q"$AsRealObj.identity[$tpe]"
      override def applyAsRaw[T: Liftable](arg: T): Tree = q"$arg"
      override def applyAsReal[T: Liftable](arg: T): Tree = q"$arg"
      override def foldWithAsReal[T: Liftable](optionLike: TermName, opt: T, default: Tree): Tree =
        q"$optionLike.getOrElse($opt, $default)"
      override def andThenAsReal[T: Liftable](func: T): Tree = q"$func"
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
      def asRaw = q"$asRawName"
      def asReal = q"$asRealName"
    }
  }

  case class MethodMapping(matchedMethod: MatchedMethod, rawMethod: RawMethod,
    paramMappingList: List[ParamMapping], resultEncoding: RpcEncoding) {

    def realMethod: RealMethod = matchedMethod.real
    def rpcName: String = matchedMethod.rpcName

    val paramMappings: Map[RawValueParam, ParamMapping] =
      paramMappingList.iterator.map(m => (m.rawParam, m)).toMap

    def ensureUniqueRpcNames(): Unit =
      paramMappings.valuesIterator.filterNot(_.rawParam.auxiliary).toList
        .flatMap(_.allMatchedParams).groupBy(_.rpcName)
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
    registerCompanionImplicits(raw.tpe)

    private def extractMapping(rawParam: RawValueParam, parser: ParamsParser): Res[ParamMapping] = {
      def createErp(matchedParam: MatchedParam, index: Int): Res[EncodedRealParam] =
        EncodedRealParam.create(rawParam, matchedParam)

      rawParam.arity match {
        case _: RpcParamArity.Single =>
          parser.extractSingle(rawParam, createErp(_, 0)).map(ParamMapping.Single(rawParam, _))
        case _: RpcParamArity.Optional =>
          Ok(ParamMapping.Optional(rawParam, parser.extractOptional(rawParam, createErp(_, 0))))
        case RpcParamArity.Multi(_, true) =>
          parser.extractMulti(rawParam, createErp, named = true).map(ParamMapping.NamedMulti(rawParam, _))
        case _: RpcParamArity.Multi if rawParam.actualType <:< BIndexedSeqTpe =>
          parser.extractMulti(rawParam, createErp, named = false).map(ParamMapping.IndexedMulti(rawParam, _))
        case _: RpcParamArity.Multi =>
          parser.extractMulti(rawParam, createErp, named = false).map(ParamMapping.IterableMulti(rawParam, _))
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
          else
            Fail(s"no encoding/decoding found between real result type " +
              s"$realResultType and raw result type ${rawMethod.resultType}")
        }

      for {
        resultConv <- resultEncoding
        paramMappings <- collectParamMappings(rawMethod.allValueParams, "raw parameter", matchedMethod)(extractMapping)
      } yield MethodMapping(matchedMethod, rawMethod, paramMappings, resultConv)
    }

    lazy val methodMappings: List[MethodMapping] =
      collectMethodMappings(raw.rawMethods, "raw methods", real.realMethods)(mappingRes)

    def ensureUniqueRpcNames(): Unit =
      methodMappings.groupBy(_.matchedMethod.rpcName).foreach {
        case (_, single :: Nil) =>
          single.ensureUniqueRpcNames()
        case (rpcName, head :: tail) =>
          head.realMethod.reportProblem(s"it has the same RPC name ($rpcName) as ${tail.size} other methods - " +
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
