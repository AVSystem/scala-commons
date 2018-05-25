package com.avsystem.commons
package macros.rpc

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait RPCMappings { this: RPCMacroCommons with RPCSymbols =>

  import c.universe._

  def collectMethodMappings[R <: RawRpcSymbol, M](rawSymbols: List[R], rawShortDesc: String, realMethods: List[RealMethod])
    (createMapping: (R, RealMethod) => Res[M]): List[M] = {

    val failedReals = new ListBuffer[String]
    def addFailure(realMethod: RealMethod, message: String): Unit = {
      errorAt(s"${realMethod.problemStr}: $message", realMethod.pos)
      failedReals += realMethod.nameStr
    }

    val result = realMethods.flatMap { realMethod =>
      val methodMappings = rawSymbols.map { rawSymbol =>
        val res = for {
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

  def collectParamMappings[R <: RawParamLike, M](raws: List[R], rawShortDesc: String, realMethod: RealMethod)
    (createMapping: (R, ParamsParser) => Res[M]): Res[List[M]] = {

    val parser = new ParamsParser(realMethod)
    Res.traverse(raws)(createMapping(_, parser)).flatMap { result =>
      if (parser.remaining.isEmpty) Ok(result.reverse)
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

    def extractSingle[B](raw: RawParamLike, matcher: RealParam => Res[B]): Res[B] = {
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
        } else Fail(s"${raw.shortDescription} ${raw.nameStr} was not matched by real parameter")
      loop()
    }

    def extractOptional[B](raw: RawParamLike, matcher: RealParam => Res[B]): Option[B] = {
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

    def extractMulti[B](raw: RawParamLike, matcher: RealParam => Res[B], named: Boolean): Res[List[B]] = {
      val seenRpcNames = new mutable.HashSet[String]
      val it = realParams.listIterator()
      def loop(result: ListBuffer[B]): Res[List[B]] =
        if (it.hasNext) {
          val real = it.next()
          if (raw.matchesTag(real)) {
            if (!raw.auxiliary) {
              it.remove()
            }
            matcher(real) match {
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
    def rawValueTree: Tree = q"${encoding.asRaw}.asRaw(${realParam.safeName})"
    def localValueDecl(body: Tree): Tree = realParam.localValueDecl(body)
  }
  object EncodedRealParam {
    def create(rawParam: RawParam, realParam: RealParam): Res[EncodedRealParam] =
      RpcEncoding.forParam(rawParam, realParam).map(EncodedRealParam(realParam, _))
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
        rawParam.mkOptional(wrapped.map(_.rawValueTree))
      def realDecls: List[Tree] =
        wrapped.toList.map { erp =>
          val defaultValueTree = erp.realParam.defaultValueTree
          erp.realParam.localValueDecl(
            q"${rawParam.optionLike}.fold(${rawParam.safeName}, $defaultValueTree)(${erp.encoding.asReal}.asReal(_))")
        }
    }
    abstract class ListedMulti extends ParamMapping {
      protected def reals: List[EncodedRealParam]
      def rawValueTree: Tree = rawParam.mkMulti(reals.map(_.rawValueTree))
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
      def rawValueTree: Tree =
        rawParam.mkMulti(reals.map(erp => q"(${erp.realParam.rpcName}, ${erp.rawValueTree})"))
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

    def rawCaseImpl: CaseDef =
      cq"""
        ${realMethod.rpcName} =>
          ..${paramMappings.filterNot(_.rawParam.auxiliary).flatMap(_.realDecls)}
          ${resultEncoding.asRaw}.asRaw(${realMethod.owner.safeName}.${realMethod.name}(...${realMethod.argLists}))
      """.asInstanceOf[CaseDef] // IntelliJ doesn't understand that cq returns CaseDef
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

    private def extractMapping(rawParam: RawParam, parser: ParamsParser): Res[ParamMapping] = {
      def createErp(realParam: RealParam): Res[EncodedRealParam] = EncodedRealParam.create(rawParam, realParam)
      rawParam.arity match {
        case _: RpcArity.Single =>
          parser.extractSingle(rawParam, createErp).map(ParamMapping.Single(rawParam, _))
        case _: RpcArity.Optional =>
          Ok(ParamMapping.Optional(rawParam, parser.extractOptional(rawParam, createErp)))
        case RpcArity.Multi(_, true) =>
          parser.extractMulti(rawParam, createErp, named = true).map(ParamMapping.NamedMulti(rawParam, _))
        case _: RpcArity.Multi if rawParam.actualType <:< BIndexedSeqTpe =>
          parser.extractMulti(rawParam, createErp, named = false).map(ParamMapping.IndexedMulti(rawParam, _))
        case _: RpcArity.Multi =>
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
        paramMappings <- collectParamMappings(rawMethod.rawParams, "raw parameter", realMethod)(extractMapping)
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
      val caseDefs = raw.rawMethods.iterator.map(rm => (rm, new mutable.LinkedHashMap[String, CaseDef])).toMap
      methodMappings.foreach { mapping =>
        val prevCaseDef = caseDefs(mapping.rawMethod).put(mapping.realMethod.rpcName, mapping.rawCaseImpl)
        if (prevCaseDef.nonEmpty) {
          mapping.realMethod.reportProblem(
            s"multiple RPCs named ${mapping.realMethod.rpcName} map to raw method ${mapping.rawMethod.nameStr}. " +
              "If you want to overload RPCs, disambiguate them with @rpcName annotation")
        }
      }

      val rawMethodImpls = raw.rawMethods.map(m => m.rawImpl(caseDefs(m).values.toList))

      q"""
        def asRaw(${real.safeName}: ${real.tpe}): ${raw.tpe} = new ${raw.tpe} {
          ..$rawMethodImpls; ()
        }
      """
    }
  }
}
