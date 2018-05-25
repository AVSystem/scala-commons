package com.avsystem.commons
package macros.rpc

trait RPCSymbols { this: RPCMacroCommons =>

  import c.universe._

  sealed abstract class RpcArity(val verbatimByDefault: Boolean) {
    def perRealParamType: Type
  }
  object RpcArity {
    def fromAnnotation(annot: Tree, param: RawParamLike): Res[RpcArity] = {
      val at = annot.tpe
      if (at <:< SingleArityAT) Ok(RpcArity.Single(param.actualType))
      else if (at <:< OptionalArityAT) {
        val optionLikeType = typeOfCachedImplicit(param.optionLike)
        val valueMember = optionLikeType.member(TypeName("Value"))
        if (valueMember.isAbstract)
          Fail("could not determine actual value of optional parameter type")
        else
          Ok(RpcArity.Optional(valueMember.typeSignatureIn(optionLikeType)))
      }
      else if (at <:< MultiArityAT) {
        if (param.actualType <:< StringPFTpe)
          Ok(NamedMulti(param.actualType.baseType(PartialFunctionClass).typeArgs(1)))
        else if (param.actualType <:< BIndexedSeqTpe)
          Ok(IndexedMulti(param.actualType.baseType(BIndexedSeqClass).typeArgs.head))
        else if (param.actualType <:< BIterableTpe)
          Ok(IterableMulti(param.actualType.baseType(BIterableClass).typeArgs.head))
        else
          Fail(s"@multi ${param.shortDescription} must be a PartialFunction of String " +
            s"(for param mapping) or Iterable (for param sequence)")
      }
      else Fail(s"unrecognized RPC arity annotation: $annot")
    }

    case class Single(perRealParamType: Type) extends RpcArity(true)
    case class Optional(perRealParamType: Type) extends RpcArity(true)
    case class IterableMulti(perRealParamType: Type) extends RpcArity(false)
    case class IndexedMulti(perRealParamType: Type) extends RpcArity(false)
    case class NamedMulti(perRealParamType: Type) extends RpcArity(false)
  }

  abstract class RpcSymbol {
    def symbol: Symbol
    def pos: Position = symbol.pos
    def shortDescription: String
    def description: String
    def problemStr: String = s"problem with $description"

    def reportProblem(msg: String, detailPos: Position = NoPosition): Nothing =
      abortAt(s"$problemStr: $msg", if (detailPos != NoPosition) detailPos else pos)

    def infer(tpt: Tree): TermName =
      infer(getType(tpt))

    def infer(tpe: Type): TermName =
      inferCachedImplicit(tpe, s"$problemStr: ", pos)

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

    lazy val requiredTag: Type = {
      val result = annot(TaggedAT).fold(baseTag)(_.tpe.baseType(TaggedAT.typeSymbol).typeArgs.head)
      if (!(result <:< baseTag)) {
        val msg =
          if (baseTag =:= NothingTpe)
            "cannot use @tagged, no tag annotation type specified with @methodTag/@paramTag"
          else s"tag annotation type $requiredTag specified in @tagged annotation " +
            s"must be a subtype of specified base tag $baseTag"
        reportProblem(msg)
      }
      result
    }

    def matchesTag(realSymbol: RealRpcSymbol): Boolean =
      realSymbol.tag(baseTag, defaultTag) <:< requiredTag

    def matchTag(realRpcSymbol: RealRpcSymbol): Res[Unit] =
      if (matchesTag(realRpcSymbol)) Ok(())
      else {
        val tag = realRpcSymbol.tag(baseTag, defaultTag)
        Fail(s"it does not accept ${realRpcSymbol.shortDescription}s tagged with $tag")
      }
  }

  sealed trait RealRpcSymbol extends RpcSymbol {
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
    def ownerType: Type

    if (!symbol.isMethod) {
      abortAt(s"problem with member $nameStr of type $ownerType: it must be a method (def)", pos)
    }

    val sig: Type = symbol.typeSignatureIn(ownerType)
    if (sig.typeParams.nonEmpty) {
      // can we relax this?
      reportProblem("RPC methods must not be generic")
    }

    def paramLists: List[List[RpcParam]]
    val resultType: Type = sig.finalResultType

    def argLists: List[List[Tree]] = paramLists.map(_.map(_.argToPass))
    def paramDecls: List[List[Tree]] = paramLists.map(_.map(_.paramDecl))
  }

  abstract class RpcParam extends RpcSymbol {
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

  case class RpcNameParam(owner: RawMethod, symbol: Symbol) extends RpcParam {
    def shortDescription = "RPC name parameter"
    def description = s"$shortDescription $nameStr of ${owner.description}"
  }

  trait RawParamLike extends RpcParam with RawRpcSymbol {
    val arity: RpcArity = annot(RpcArityAT)
      .map(a => RpcArity.fromAnnotation(a.tree, this).getOrElse(reportProblem(_)))
      .getOrElse(RpcArity.Single(actualType))

    val verbatim: Boolean =
      annot(RpcEncodingAT).map(_.tpe <:< VerbatimAT).getOrElse(arity.verbatimByDefault)

    val auxiliary: Boolean =
      annot(AuxiliaryAT).nonEmpty

    lazy val optionLike: TermName = infer(tq"$OptionLikeCls[$actualType]")

    lazy val canBuildFrom: TermName = arity match {
      case _: RpcArity.NamedMulti =>
        infer(tq"$CanBuildFromCls[$NothingCls,($StringCls,${arity.perRealParamType}),$actualType]")
      case _: RpcArity.IndexedMulti | _: RpcArity.IterableMulti =>
        infer(tq"$CanBuildFromCls[$NothingCls,${arity.perRealParamType},$actualType]")
      case _ => abort(s"(bug) CanBuildFrom computed for non-multi $shortDescription")
    }

    def cannotMapClue: String
  }

  case class RawParam(owner: RawMethod, symbol: Symbol) extends RawParamLike {
    def baseTag: Type = owner.baseParamTag
    def defaultTag: Type = owner.defaultParamTag

    def shortDescription = "raw parameter"
    def description = s"$shortDescription $nameStr of ${owner.description}"
    def cannotMapClue = s"cannot map it to $shortDescription $nameStr of ${owner.nameStr}"
  }

  case class RealParam(owner: RealMethod, symbol: Symbol, index: Int, indexInList: Int) extends RpcParam with RealRpcSymbol {
    def shortDescription = "real parameter"
    def description = s"$shortDescription $nameStr of ${owner.description}"

    def defaultValueTree: Tree =
      if (symbol.asTerm.isParamWithDefault) {
        val prevListParams = owner.realParams.take(index - indexInList).map(rp => q"${rp.safeName}")
        val prevListParamss = List(prevListParams).filter(_.nonEmpty)
        q"${owner.owner.safeName}.${TermName(s"${owner.encodedNameStr}$$default$$${index + 1}")}(...$prevListParamss)"
      }
      else q"$RpcPackage.RpcUtils.missingArg(${owner.rpcName}, $rpcName)"
  }

  case class RawMethod(owner: RawRpcTrait, symbol: Symbol) extends RpcMethod with RawRpcSymbol {
    def shortDescription = "raw method"
    def description = s"$shortDescription $nameStr of ${owner.description}"

    def ownerType: Type = owner.tpe
    def baseTag: Type = owner.baseMethodTag
    def defaultTag: Type = owner.defaultMethodTag

    // raw method result is encoded by default, must be explicitly annotated as @verbatim to disable encoding
    val verbatimResult: Boolean =
      annot(RpcEncodingAT).exists(_.tpe <:< VerbatimAT)

    val List(baseParamTag, defaultParamTag) =
      annot(ParamTagAT)
        .map(_.tpe.baseType(ParamTagAT.typeSymbol).typeArgs)
        .getOrElse(List(owner.baseParamTag, owner.defaultParamTag))

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

  case class RealMethod(owner: RealRpcTrait, symbol: Symbol) extends RpcMethod with RealRpcSymbol {
    def ownerType: Type = owner.tpe

    def shortDescription = "real method"
    def description = s"$shortDescription $nameStr of ${owner.description}"

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
    def shortDescription = "raw RPC"
    def description = s"$shortDescription $tpe"

    def baseTag: Type = typeOf[Nothing]
    def defaultTag: Type = typeOf[Nothing]

    val List(baseMethodTag, defaultMethodTag) =
      annot(MethodTagAT)
        .map(_.tpe.baseType(MethodTagAT.typeSymbol).typeArgs)
        .getOrElse(List(NothingTpe, NothingTpe))

    val List(baseParamTag, defaultParamTag) =
      annot(ParamTagAT)
        .map(_.tpe.baseType(ParamTagAT.typeSymbol).typeArgs)
        .getOrElse(List(NothingTpe, NothingTpe))

    lazy val rawMethods: List[RawMethod] =
      tpe.members.iterator.filter(m => m.isTerm && m.isAbstract).map(RawMethod(this, _)).toList
  }

  case class RealRpcTrait(tpe: Type)
    extends RpcTrait(tpe.typeSymbol) with RealRpcSymbol {

    def shortDescription = "real RPC"
    def description = s"$shortDescription $tpe"

    lazy val realMethods: List[RealMethod] =
      tpe.members.iterator.filter(m => m.isTerm && m.isAbstract).map(RealMethod(this, _)).toList
  }
}
