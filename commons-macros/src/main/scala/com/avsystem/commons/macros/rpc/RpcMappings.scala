package com.avsystem.commons
package macros.rpc

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait RpcMappings { this: RpcMacroCommons with RpcSymbols =>

  import c.universe._

  def collectMethodMappings[R <: RawRpcSymbol with AritySymbol, M](
    rawSymbols: List[R], rawShortDesc: String, realMethods: List[RealMethod])(
    createMapping: (R, RealMethod) => Res[M]): List[M] = {

    val failedReals = new ListBuffer[String]
    def addFailure(realMethod: RealMethod, message: String): Unit = {
      errorAt(s"${realMethod.problemStr}: $message", realMethod.pos)
      failedReals += realMethod.nameStr
    }

    val result = realMethods.flatMap { realMethod =>
      val methodMappings = rawSymbols.map { rawSymbol =>
        val res = for {
          _ <- rawSymbol.matchName(realMethod)
          _ <- rawSymbol.matchTag(realMethod)
          methodMapping <- createMapping(rawSymbol, realMethod)
        } yield (rawSymbol, methodMapping)
        res.mapFailure(msg => s"${rawSymbol.shortDescription} ${rawSymbol.nameStr} did not match: $msg")
      }
      methodMappings.collect { case Ok(m) => m } match {
        case List((_, m)) => Some(m)
        case Nil =>
          val unmatchedReport = methodMappings.iterator.collect({ case Fail(error) => s" * $error" }).mkString("\n")
          addFailure(realMethod, s"it has no matching $rawShortDesc:\n$unmatchedReport")
          None
        case multiple =>
          addFailure(realMethod, s"it has multiple matching $rawShortDesc: ${multiple.map(_._1.nameStr).mkString(",")}")
          None
      }
    }

    if (failedReals.nonEmpty) {
      abort(s"Following real methods could not be mapped to $rawShortDesc: ${failedReals.mkString(",")}")
    }

    result
  }

  def collectParamMappings[R <: RealParamTarget, M](raws: List[R], rawShortDesc: String, realMethod: RealMethod)
    (createMapping: (R, ParamsParser) => Res[M]): Res[List[M]] = {

    val parser = new ParamsParser(realMethod)
    Res.traverse(raws)(createMapping(_, parser)).flatMap { result =>
      if (parser.remaining.isEmpty) Ok(result)
      else {
        val unmatched = parser.remaining.iterator.map(_.nameStr).mkString(",")
        Fail(s"no $rawShortDesc(s) were found that would match real parameter(s) $unmatched")
      }
    }
  }

  class ParamsParser(realMethod: RealMethod) {

    import scala.collection.JavaConverters._

    private val realParams = new java.util.LinkedList[RealParam]
    realParams.addAll(realMethod.realParams.asJava)

    def remaining: Seq[RealParam] = realParams.asScala

    def extractSingle[B](raw: RealParamTarget, matcher: RealParam => Res[B]): Res[B] = {
      val it = realParams.listIterator()
      def loop(): Res[B] =
        if (it.hasNext) {
          val real = it.next()
          if (raw.matchesTag(real)) {
            if (!raw.auxiliary) {
              it.remove()
            }
            matcher(real)
          } else loop()
        } else Fail(s"${raw.shortDescription} ${raw.pathStr} was not matched by real parameter")
      loop()
    }

    def extractOptional[B](raw: RealParamTarget, matcher: RealParam => Res[B]): Option[B] = {
      val it = realParams.listIterator()
      def loop(): Option[B] =
        if (it.hasNext) {
          val real = it.next()
          if (raw.matchesTag(real)) {
            val res = matcher(real).toOption
            if (!raw.auxiliary) {
              res.foreach(_ => it.remove())
            }
            res
          } else loop()
        } else None
      loop()
    }

    def extractMulti[B](raw: RealParamTarget, matcher: (RealParam, Int) => Res[B], named: Boolean): Res[List[B]] = {
      val seenRpcNames = new mutable.HashSet[String]
      val it = realParams.listIterator()
      def loop(result: ListBuffer[B]): Res[List[B]] =
        if (it.hasNext) {
          val real = it.next()
          if (raw.matchesTag(real)) {
            if (!raw.auxiliary) {
              it.remove()
            }
            matcher(real, result.size) match {
              case Ok(b) =>
                result += b
                if (named && !seenRpcNames.add(real.rpcName)) {
                  realMethod.reportProblem(s"multiple parameters matched to ${raw.shortDescription} ${raw.nameStr} " +
                    s"have the same @rpcName: ${real.rpcName}")
                }
                loop(result)
              case fail: Fail =>
                fail
            }
          } else loop(result)
        } else Ok(result.result())
      loop(new ListBuffer[B])
    }
  }

  case class EncodedRealParam(realParam: RealParam, encoding: RpcEncoding) {
    def safeName: TermName = realParam.safeName
    def rawValueTree: Tree = encoding.applyAsRaw(realParam.safeName)
    def localValueDecl(body: Tree): Tree = realParam.localValueDecl(body)
  }
  object EncodedRealParam {
    def create(rawParam: RawValueParam, realParam: RealParam): Res[EncodedRealParam] =
      RpcEncoding.forParam(rawParam, realParam).map(EncodedRealParam(realParam, _))
  }

  sealed trait ParamMapping {
    def rawParam: RawValueParam
    def rawValueTree: Tree
    def realDecls: List[Tree]
  }
  object ParamMapping {
    case class Single(rawParam: RawValueParam, realParam: EncodedRealParam) extends ParamMapping {
      def rawValueTree: Tree =
        realParam.rawValueTree
      def realDecls: List[Tree] =
        List(realParam.localValueDecl(realParam.encoding.applyAsReal(rawParam.safePath)))
    }
    case class Optional(rawParam: RawValueParam, wrapped: Option[EncodedRealParam]) extends ParamMapping {
      def rawValueTree: Tree =
        rawParam.mkOptional(wrapped.map(_.rawValueTree))
      def realDecls: List[Tree] = wrapped.toList.map { erp =>
        val defaultValueTree = erp.realParam.defaultValueTree
        erp.realParam.localValueDecl(erp.encoding.foldWithAsReal(
          rawParam.optionLike, rawParam.safePath, defaultValueTree))
      }
    }
    abstract class ListedMulti extends ParamMapping {
      protected def reals: List[EncodedRealParam]
      def rawValueTree: Tree = rawParam.mkMulti(reals.map(_.rawValueTree))
    }
    case class IterableMulti(rawParam: RawValueParam, reals: List[EncodedRealParam]) extends ListedMulti {
      def realDecls: List[Tree] = {
        val itName = c.freshName(TermName("it"))
        val itDecl = q"val $itName = ${rawParam.safePath}.iterator"
        itDecl :: reals.map { erp =>
          val rp = erp.realParam
          if (rp.symbol.asTerm.isByNameParam) {
            rp.reportProblem(
              s"${rawParam.cannotMapClue}: by-name real parameters cannot be extracted from @multi raw parameters")
          }
          erp.localValueDecl(
            q"if($itName.hasNext) ${erp.encoding.applyAsReal(q"$itName.next()")} else ${rp.defaultValueTree}")
        }
      }
    }
    case class IndexedMulti(rawParam: RawValueParam, reals: List[EncodedRealParam]) extends ListedMulti {
      def realDecls: List[Tree] = {
        reals.zipWithIndex.map { case (erp, idx) =>
          val rp = erp.realParam
          erp.realParam.localValueDecl(
            q"""
              ${erp.encoding.andThenAsReal(rawParam.safePath)}
                .applyOrElse($idx, (_: $IntCls) => ${rp.defaultValueTree})
            """)
        }
      }
    }
    case class NamedMulti(rawParam: RawValueParam, reals: List[EncodedRealParam]) extends ParamMapping {
      def rawValueTree: Tree =
        rawParam.mkMulti(reals.map(erp => q"(${erp.realParam.rpcName}, ${erp.rawValueTree})"))
      def realDecls: List[Tree] =
        reals.map { erp =>
          erp.realParam.localValueDecl(
            q"""
              ${erp.encoding.andThenAsReal(rawParam.safePath)}
                .applyOrElse(${erp.realParam.rpcName}, (_: $StringCls) => ${erp.realParam.defaultValueTree})
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

  case class MethodMapping(realMethod: RealMethod, rawMethod: RawMethod,
    paramMappingList: List[ParamMapping], resultEncoding: RpcEncoding) {

    val paramMappings: Map[RawValueParam, ParamMapping] =
      paramMappingList.iterator.map(m => (m.rawParam, m)).toMap

    private def rawValueTree(rawParam: RawParam): Tree = rawParam match {
      case rvp: RawValueParam => paramMappings(rvp).rawValueTree
      case crp: CompositeRawParam =>
        q"""
          ..${crp.paramLists.flatten.map(p => p.localValueDecl(rawValueTree(p)))}
          new ${crp.actualType}(...${crp.paramLists.map(_.map(_.safeName))})
         """
    }

    def realImpl: Tree = {
      val rpcNameParamDecl: Option[Tree] = rawMethod.arity match {
        case RpcMethodArity.Multi(rpcNameParam) =>
          Some(q"val ${rpcNameParam.safeName} = ${realMethod.rpcName}")
        case RpcMethodArity.Single | RpcMethodArity.Optional =>
          None
      }

      q"""
        def ${realMethod.name}(...${realMethod.paramDecls}): ${realMethod.resultType} = {
          ..${rpcNameParamDecl.toList}
          ..${rawMethod.rawParams.getOrElse(Nil).map(rp => rp.localValueDecl(rawValueTree(rp)))}
          ${resultEncoding.applyAsReal(q"${rawMethod.owner.safeName}.${rawMethod.name}(...${rawMethod.argLists})")}
        }
       """
    }

    def rawCaseImpl: Tree =
      q"""
        ..${paramMappings.values.filterNot(_.rawParam.auxiliary).flatMap(_.realDecls)}
        ${resultEncoding.applyAsRaw(q"${realMethod.owner.safeName}.${realMethod.name}(...${realMethod.argLists})")}
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
      def createErp(realParam: RealParam, index: Int): Res[EncodedRealParam] = EncodedRealParam.create(rawParam, realParam)
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

    private def mappingRes(rawMethod: RawMethod, realMethod: RealMethod): Res[MethodMapping] = {
      def resultEncoding: Res[RpcEncoding] =
        if (rawMethod.verbatimResult) {
          if (rawMethod.resultType =:= realMethod.resultType)
            Ok(RpcEncoding.Verbatim(rawMethod.resultType))
          else
            Fail(s"real result type ${realMethod.resultType} does not match raw result type ${rawMethod.resultType}")
        } else {
          val e = RpcEncoding.RealRawEncoding(realMethod.resultType, rawMethod.resultType, None)
          if ((!forAsRaw || e.asRawName != termNames.EMPTY) && (!forAsReal || e.asRealName != termNames.EMPTY))
            Ok(e)
          else
            Fail(s"no encoding/decoding found between real result type " +
              s"${realMethod.resultType} and raw result type ${rawMethod.resultType}")
        }

      for {
        resultConv <- resultEncoding
        paramMappings <- collectParamMappings(rawMethod.allValueParams, "raw parameter", realMethod)(extractMapping)
      } yield MethodMapping(realMethod, rawMethod, paramMappings, resultConv)
    }

    lazy val methodMappings: List[MethodMapping] =
      collectMethodMappings(raw.rawMethods, "raw methods", real.realMethods)(mappingRes)

    def asRealImpl: Tree =
      q"""
        def asReal(${raw.safeName}: ${raw.tpe}): ${real.tpe} = new ${real.tpe} {
          ..${methodMappings.map(_.realImpl)}; ()
        }
      """

    def asRawImpl: Tree = {
      val caseImpls = raw.rawMethods.iterator.map(rm => (rm, new mutable.LinkedHashMap[String, Tree])).toMap
      methodMappings.foreach { mapping =>
        val prevCaseDef = caseImpls(mapping.rawMethod).put(mapping.realMethod.rpcName, mapping.rawCaseImpl)
        if (prevCaseDef.nonEmpty) {
          mapping.realMethod.reportProblem(
            s"multiple RPCs named ${mapping.realMethod.rpcName} map to raw method ${mapping.rawMethod.nameStr}. " +
              "If you want to overload RPCs, disambiguate them with @rpcName annotation")
        }
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
