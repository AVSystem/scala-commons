package com.avsystem.commons
package macros.rpc

import java.util.LinkedList

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait RPCMappings { this: RPCMacroCommons with RPCSymbols =>

  import c.universe._

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

    if (forAsReal) {
      registerImplicit(getType(tq"$AsRealCls[${raw.tpe},${real.tpe}]"), selfName)
    }
    if (forAsRaw) {
      registerImplicit(getType(tq"$AsRawCls[${raw.tpe},${real.tpe}]"), selfName)
    }
    registerCompanionImplicits(raw.tpe)

    private def extractMapping(rawParam: RawParam, realParams: LinkedList[RealParam]): Res[ParamMapping] = {
      val it = realParams.listIterator()
      def maybeConsume(): Unit =
        if (!rawParam.auxiliary) {
          it.remove()
        }

      def extractSingle(): Res[EncodedRealParam] =
        if (it.hasNext) {
          val realParam = it.next()
          if (rawParam.matchesTag(realParam)) {
            maybeConsume()
            RpcEncoding.forParam(rawParam, realParam).map(e => EncodedRealParam(realParam, e))
          } else extractSingle()
        } else {
          rawParam.owner.matchFailure(s"raw parameter ${rawParam.nameStr} was not matched by real parameter")
        }

      def extractOptional(): Option[EncodedRealParam] =
        if (it.hasNext) {
          val realParam = it.next()
          if (rawParam.matchesTag(realParam)) {
            RpcEncoding.forParam(rawParam, realParam).toOption.map { encoding =>
              maybeConsume()
              EncodedRealParam(realParam, encoding)
            }
          } else extractOptional()
        } else None

      def extractMulti() = {
        def loop(): Res[List[EncodedRealParam]] =
          if (it.hasNext) {
            val realParam = it.next()
            if (rawParam.matchesTag(realParam)) {
              maybeConsume()
              for {
                encoding <- RpcEncoding.forParam(rawParam, realParam)
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
            s"multiple parameters matched to raw parameter ${rawParam.nameStr} have the same @rpcName: $rpcName")
        }
        result
      }

      rawParam.arity match {
        case _: RpcArity.Single => extractSingle().map(ParamMapping.Single(rawParam, _))
        case _: RpcArity.Optional => Ok(ParamMapping.Optional(rawParam, extractOptional()))
        case _: RpcArity.IterableMulti => extractMulti().map(ParamMapping.IterableMulti(rawParam, _))
        case _: RpcArity.IndexedMulti => extractMulti().map(ParamMapping.IndexedMulti(rawParam, _))
        case _: RpcArity.NamedMulti => extractMulti().map(ParamMapping.NamedMulti(rawParam, _))
      }
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
            mapping <- extractMapping(rawParam, realBuf)
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
            val unmatchedReport = methodMappings.iterator.collect({ case Fail(error) => s" * $error" }).mkString("\n")
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
        if (prevCaseDef.nonEmpty) {
          mapping.realMethod.reportProblem(
            s"multiple RPCs named ${mapping.realMethod.rpcName} map to raw method ${mapping.rawMethod.name}. " +
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
