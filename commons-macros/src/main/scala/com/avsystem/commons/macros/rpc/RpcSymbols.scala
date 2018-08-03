package com.avsystem.commons
package macros.rpc

trait RpcSymbols { this: RpcMacroCommons =>

  import c.universe._

  sealed abstract class RpcArity
  object RpcArity {
    trait Single extends RpcArity
    trait Optional extends RpcArity
    trait Multi extends RpcArity
  }

  sealed abstract class RpcParamArity(val verbatimByDefault: Boolean) extends RpcArity {
    def collectedType: Type
  }
  object RpcParamArity {
    def fromAnnotation(param: ArityParam,
      allowMulti: Boolean, allowListed: Boolean, allowNamed: Boolean): RpcParamArity = {

      val at = findAnnotation(param.symbol, RpcArityAT).fold(SingleArityAT)(_.tpe)
      if (at <:< SingleArityAT) RpcParamArity.Single(param.actualType)
      else if (at <:< OptionalArityAT) {
        val optionLikeType = typeOfCachedImplicit(param.optionLike)
        val valueMember = optionLikeType.member(TypeName("Value"))
        if (valueMember.isAbstract)
          param.reportProblem("could not determine actual value of optional parameter type")
        else
          RpcParamArity.Optional(valueMember.typeSignatureIn(optionLikeType))
      }
      else if (allowMulti && at <:< MultiArityAT) {
        if (allowNamed && param.actualType <:< StringPFTpe)
          Multi(param.actualType.baseType(PartialFunctionClass).typeArgs(1), named = true)
        else if (allowListed && param.actualType <:< BIterableTpe)
          Multi(param.actualType.baseType(BIterableClass).typeArgs.head, named = false)
        else if (allowNamed && allowListed)
          param.reportProblem(s"@multi ${param.shortDescription} must be a PartialFunction of String " +
            s"(for by-name mapping) or Iterable (for sequence)")
        else if (allowListed)
          param.reportProblem(s"@multi ${param.shortDescription} must be an Iterable")
        else
          param.reportProblem(s"@multi ${param.shortDescription} must be a PartialFunction of String")
      }
      else param.reportProblem(s"forbidden RPC arity annotation: $at")
    }

    case class Single(collectedType: Type) extends RpcParamArity(true) with RpcArity.Single
    case class Optional(collectedType: Type) extends RpcParamArity(true) with RpcArity.Optional
    case class Multi(collectedType: Type, named: Boolean) extends RpcParamArity(false) with RpcArity.Multi
  }

  sealed abstract class RpcMethodArity(val verbatimByDefault: Boolean) extends RpcArity
  object RpcMethodArity {
    def fromAnnotation(method: RawMethod): RpcMethodArity = {
      val at = method.annot(RpcArityAT).fold(SingleArityAT)(_.tpe)
      if (at <:< SingleArityAT) Single
      else if (at <:< OptionalArityAT) Optional
      else if (at <:< MultiArityAT) Multi
      else method.reportProblem(s"unrecognized RPC arity annotation: $at")
    }

    case object Single extends RpcMethodArity(true) with RpcArity.Single
    case object Optional extends RpcMethodArity(true) with RpcArity.Optional
    case object Multi extends RpcMethodArity(false) with RpcArity.Multi
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

    override def equals(other: Any): Boolean = other match {
      case rpcSym: RpcSymbol => symbol == rpcSym.symbol
      case _ => false
    }
    override def hashCode: Int = symbol.hashCode
    override def toString: String = symbol.toString
  }

  case class FallbackTag(annotTree: Tree) {
    def asList: List[Tree] = List(annotTree).filter(_ != EmptyTree)
    def orElse(other: FallbackTag): FallbackTag = FallbackTag(annotTree orElse other.annotTree)
  }
  object FallbackTag {
    final val Empty = FallbackTag(EmptyTree)
  }

  sealed trait MatchedRealSymbol[+Real <: RealRpcSymbol] {
    def real: Real
    def fallbackTagUsed: FallbackTag

    def annot(tpe: Type): Option[Annot] =
      findAnnotation(real.symbol, tpe, fallback = fallbackTagUsed.asList)

    def allAnnots(tpe: Type): List[Annot] =
      allAnnotations(real.symbol, tpe, fallback = fallbackTagUsed.asList)

    val rpcName: String = {
      val prefixes = allAnnotations(real.symbol, RpcNamePrefixAT, fallback = fallbackTagUsed.asList)
        .map(_.findArg[String](RpcNamePrefixArg))
      val rpcName = annot(RpcNameAT).fold(real.nameStr)(_.findArg[String](RpcNameArg))
      prefixes.mkString("", "", rpcName)
    }
  }

  case class MatchedRpcTrait(real: RealRpcTrait) extends MatchedRealSymbol[RealRpcTrait] {
    def fallbackTagUsed: FallbackTag = FallbackTag.Empty
  }

  case class MatchedMethod(real: RealMethod, fallbackTagUsed: FallbackTag)
    extends MatchedRealSymbol[RealMethod]

  case class MatchedParam(real: RealParam, fallbackTagUsed: FallbackTag, matchedOwner: MatchedMethod)
    extends MatchedRealSymbol[RealParam] {

    val whenAbsent: Tree =
      annot(WhenAbsentAT).fold(EmptyTree) { annot =>
        val annotatedDefault = annot.tree.children.tail.head
        if (!(annotatedDefault.tpe <:< real.actualType)) {
          real.reportProblem(s"expected value of type ${real.actualType} in @whenAbsent annotation, " +
            s"got ${annotatedDefault.tpe.widen}")
        }
        val transformer = new Transformer {
          override def transform(tree: Tree): Tree = tree match {
            case Super(t@This(_), _) if !enclosingClasses.contains(t.symbol) =>
              real.reportProblem(s"illegal super-reference in @whenAbsent annotation")
            case This(_) if tree.symbol == real.owner.owner.symbol => q"${real.owner.owner.safeName}"
            case This(_) if !enclosingClasses.contains(tree.symbol) =>
              real.reportProblem(s"illegal this-reference in @whenAbsent annotation")
            case t => super.transform(t)
          }
        }
        transformer.transform(annotatedDefault)
      }

    val hasDefaultValue: Boolean =
      whenAbsent != EmptyTree || real.symbol.asTerm.isParamWithDefault

    val transientDefault: Boolean =
      hasDefaultValue && annot(TransientDefaultAT).nonEmpty

    def fallbackValueTree: Tree =
      if (whenAbsent != EmptyTree) c.untypecheck(whenAbsent)
      else if (real.symbol.asTerm.isParamWithDefault) defaultValue(false)
      else q"$RpcUtils.missingArg(${matchedOwner.rpcName}, $rpcName)"

    def transientValueTree: Tree =
      if (real.symbol.asTerm.isParamWithDefault) defaultValue(true)
      else c.untypecheck(whenAbsent)

    private def defaultValue(useThis: Boolean): Tree = {
      val prevListParams = real.owner.realParams.take(real.index - real.indexInList).map(rp => q"${rp.safeName}")
      val prevListParamss = List(prevListParams).filter(_.nonEmpty)
      val realInst = if (useThis) q"this" else q"${real.owner.owner.safeName}"
      q"$realInst.${TermName(s"${real.owner.encodedNameStr}$$default$$${real.index + 1}")}(...$prevListParamss)"
    }
  }

  trait RawRpcSymbol extends RpcSymbol {
    def baseTagTpe: Type
    def fallbackTag: FallbackTag

    def annot(tpe: Type): Option[Annot] =
      findAnnotation(symbol, tpe)

    def tagSpec(a: Annot): (Type, FallbackTag) = {
      val tagType = a.tpe.dealias.typeArgs.head
      val defaultTagArg = a.tpe.member(TermName("defaultTag"))
      val fallbackTag = FallbackTag(a.findArg[Tree](defaultTagArg, EmptyTree))
      (tagType, fallbackTag)
    }

    lazy val (requiredTag, whenUntaggedTag) = {
      val taggedAnnot = annot(TaggedAT)
      val requiredTagType = taggedAnnot.fold(baseTagTpe)(_.tpe.typeArgs.head)
      if (!(requiredTagType <:< baseTagTpe)) {
        val msg =
          if (baseTagTpe =:= NothingTpe)
            "cannot use @tagged, no tag annotation type specified with @methodTag/@paramTag"
          else s"tag annotation type $requiredTagType specified in @tagged annotation " +
            s"must be a subtype of specified base tag $baseTagTpe"
        reportProblem(msg)
      }
      val whenUntagged = FallbackTag(taggedAnnot.map(_.findArg[Tree](WhenUntaggedArg, EmptyTree)).getOrElse(EmptyTree))
      (requiredTagType, whenUntagged)
    }

    // returns fallback tag tree only IF it was necessary
    def matchTag(realRpcSymbol: RealRpcSymbol): Res[FallbackTag] = {
      val tagAnnot = findAnnotation(realRpcSymbol.symbol, baseTagTpe)
      val fallbackTagUsed = if (tagAnnot.isEmpty) whenUntaggedTag orElse fallbackTag else FallbackTag.Empty
      val realTagTpe = tagAnnot.map(_.tpe).getOrElse(NoType) orElse fallbackTagUsed.annotTree.tpe orElse baseTagTpe

      if (realTagTpe <:< requiredTag) Ok(fallbackTagUsed)
      else Fail(s"it does not accept ${realRpcSymbol.shortDescription}s tagged with $realTagTpe")
    }
  }

  sealed trait RealRpcSymbol extends RpcSymbol

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

  trait AritySymbol extends RpcSymbol {
    val arity: RpcArity

    // @unchecked because "The outer reference in this type test cannot be checked at runtime"
    // Srsly scalac, from static types it should be obvious that outer references are the same
    def matchName(matchedReal: MatchedRealSymbol[RealRpcSymbol]): Res[Unit] = arity match {
      case _: RpcArity.Single@unchecked | _: RpcArity.Optional@unchecked =>
        if (matchedReal.rpcName == nameStr) Ok(())
        else Fail(s"it only matches ${matchedReal.real.shortDescription}s with RPC name $nameStr")
      case _: RpcArity.Multi@unchecked => Ok(())
    }
  }

  trait ArityParam extends RpcParam with AritySymbol {
    def allowMulti: Boolean
    def allowNamedMulti: Boolean
    def allowListedMulti: Boolean

    val arity: RpcParamArity =
      RpcParamArity.fromAnnotation(this, allowMulti, allowListedMulti, allowNamedMulti)

    lazy val optionLike: TermName = infer(tq"$OptionLikeCls[$actualType]")

    lazy val canBuildFrom: TermName = arity match {
      case _: RpcParamArity.Multi if allowNamedMulti && actualType <:< StringPFTpe =>
        infer(tq"$CanBuildFromCls[$NothingCls,($StringCls,${arity.collectedType}),$actualType]")
      case _: RpcParamArity.Multi =>
        infer(tq"$CanBuildFromCls[$NothingCls,${arity.collectedType},$actualType]")
      case _ => abort(s"(bug) CanBuildFrom computed for non-multi $shortDescription")
    }

    def mkOptional[T: Liftable](opt: Option[T]): Tree =
      opt.map(t => q"$optionLike.some($t)").getOrElse(q"$optionLike.none")

    def mkMulti[T: Liftable](elements: List[T]): Tree =
      if (elements.isEmpty) q"$RpcUtils.createEmpty($canBuildFrom)"
      else {
        val builderName = c.freshName(TermName("builder"))
        q"""
          val $builderName = $RpcUtils.createBuilder($canBuildFrom, ${elements.size})
          ..${elements.map(t => q"$builderName += $t")}
          $builderName.result()
        """
      }
  }

  trait RealParamTarget extends ArityParam with RawRpcSymbol {
    def allowMulti: Boolean = true
    def allowNamedMulti: Boolean = true
    def allowListedMulti: Boolean = true

    def pathStr: String

    val verbatim: Boolean =
      annot(RpcEncodingAT).map(_.tpe <:< VerbatimAT).getOrElse(arity.verbatimByDefault)

    val auxiliary: Boolean =
      annot(AuxiliaryAT).nonEmpty

    def cannotMapClue: String
  }

  object RawParam {
    def apply(owner: Either[RawMethod, CompositeRawParam], symbol: Symbol): RawParam =
      if (findAnnotation(symbol, MethodNameAT).nonEmpty)
        MethodNameParam(owner, symbol)
      else if (findAnnotation(symbol, CompositeAT).nonEmpty)
        CompositeRawParam(owner, symbol)
      else RawValueParam(owner, symbol)
  }

  sealed trait RawParam extends RpcParam {
    val owner: Either[RawMethod, CompositeRawParam]
    val containingRawMethod: RawMethod = owner.fold(identity, _.containingRawMethod)

    def safePath: Tree = owner.fold(_ => q"$safeName", _.safeSelect(this))
    def pathStr: String = owner.fold(_ => nameStr, cp => s"${cp.pathStr}.$nameStr")

    def shortDescription = "raw parameter"
    def description = s"$shortDescription $nameStr of ${owner.fold(_.description, _.description)}"
  }

  case class MethodNameParam(owner: Either[RawMethod, CompositeRawParam], symbol: Symbol) extends RawParam {
    if (!(actualType =:= typeOf[String])) {
      reportProblem("@methodName parameter must be of type String")
    }
  }

  case class CompositeRawParam(owner: Either[RawMethod, CompositeRawParam], symbol: Symbol) extends RawParam {
    val constructorSig: Type = primaryConstructorOf(actualType, problemStr).typeSignatureIn(actualType)

    val paramLists: List[List[RawParam]] =
      constructorSig.paramLists.map(_.map(RawParam(Right(this), _)))

    def allLeafParams: Iterator[RawParam] = paramLists.iterator.flatten.flatMap {
      case crp: CompositeRawParam => crp.allLeafParams
      case other => Iterator(other)
    }

    def safeSelect(subParam: RawParam): Tree = {
      if (!alternatives(actualType.member(subParam.name)).exists(s => s.isMethod && s.asTerm.isParamAccessor)) {
        subParam.reportProblem(s"it is not a public member and cannot be accessed, turn it into a val")
      }
      q"$safePath.${subParam.name}"
    }
  }

  case class RawValueParam(owner: Either[RawMethod, CompositeRawParam], symbol: Symbol)
    extends RawParam with RealParamTarget {

    def baseTagTpe: Type = containingRawMethod.baseParamTag
    def fallbackTag: FallbackTag = containingRawMethod.fallbackParamTag

    def cannotMapClue = s"cannot map it to $shortDescription $pathStr of ${containingRawMethod.nameStr}"
  }

  case class RealParam(owner: RealMethod, symbol: Symbol, index: Int, indexOfList: Int, indexInList: Int)
    extends RpcParam with RealRpcSymbol {

    def shortDescription = "real parameter"
    def description = s"$shortDescription $nameStr of ${owner.description}"
  }

  case class RawMethod(owner: RawRpcTrait, symbol: Symbol) extends RpcMethod with RawRpcSymbol with AritySymbol {
    def shortDescription = "raw method"
    def description = s"$shortDescription $nameStr of ${owner.description}"

    def ownerType: Type = owner.tpe
    def baseTagTpe: Type = owner.baseMethodTag
    def fallbackTag: FallbackTag = owner.fallbackMethodTag

    val arity: RpcMethodArity = RpcMethodArity.fromAnnotation(this)
    val tried: Boolean = annot(TriedAT).nonEmpty

    val verbatimResult: Boolean =
      annot(RpcEncodingAT).map(_.tpe <:< VerbatimAT).getOrElse(arity.verbatimByDefault)

    val (baseParamTag, fallbackParamTag) =
      annot(ParamTagAT).map(tagSpec).getOrElse(owner.baseParamTag, owner.fallbackParamTag)

    val rawParams: List[RawParam] = sig.paramLists match {
      case Nil | List(_) => sig.paramLists.flatten.map(RawParam(Left(this), _))
      case _ => reportProblem(s"raw methods cannot take multiple parameter lists")
    }

    val paramLists: List[List[RawParam]] =
      if (sig.paramLists.isEmpty) Nil else List(rawParams)

    def allLeafParams: Iterator[RawParam] = rawParams.iterator.flatMap {
      case crp: CompositeRawParam => crp.allLeafParams
      case other => Iterator(other)
    }

    val allValueParams: List[RawValueParam] =
      allLeafParams.collect({ case rvp: RawValueParam => rvp }).toList

    lazy val methodNameParam: MethodNameParam =
      allLeafParams.collectFirst({ case mnp: MethodNameParam => mnp })
        .getOrElse(reportProblem("no @methodName parameter found on @multi raw method"))

    def rawImpl(caseDefs: List[(String, Tree)]): Tree = {
      val body = arity match {
        case RpcMethodArity.Single => caseDefs match {
          case Nil => abort(s"no real method found that would match $description")
          case List((_, single)) => single
          case _ => abort(s"multiple real methods match $description")
        }
        case RpcMethodArity.Optional => caseDefs match {
          case Nil => q"$RpcUtils.missingOptionalRpc($nameStr)"
          case List((_, single)) => single
          case _ => abort(s"multiple real methods match $description")
        }
        case RpcMethodArity.Multi =>
          val methodNameName = c.freshName(TermName("methodName"))
          q"""
            ${methodNameParam.safePath} match {
              case ..${caseDefs.map({ case (rpcName, tree) => cq"$rpcName => $tree" })}
              case $methodNameName => $RpcUtils.unknownRpc($methodNameName, $nameStr)
            }
           """
      }
      q"def $name(...$paramDecls): $resultType = $body"
    }
  }

  case class RealMethod(owner: RealRpcTrait, symbol: Symbol) extends RpcMethod with RealRpcSymbol {
    def ownerType: Type = owner.tpe

    def shortDescription = "real method"
    def description = s"$shortDescription $nameStr"

    val paramLists: List[List[RealParam]] = {
      var index = 0
      var indexOfList = 0
      sig.paramLists.map { ss =>
        var indexInList = 0
        val res = ss.map { s =>
          val res = RealParam(this, s, index, indexOfList, indexInList)
          index += 1
          indexInList += 1
          res
        }
        indexOfList += 1
        res
      }
    }

    val realParams: List[RealParam] = paramLists.flatten
  }

  case class RawRpcTrait(tpe: Type) extends RpcTrait(tpe.typeSymbol) with RawRpcSymbol {
    def shortDescription = "raw RPC"
    def description = s"$shortDescription $tpe"

    def baseTagTpe: Type = NothingTpe
    def fallbackTag: FallbackTag = FallbackTag.Empty

    val (baseMethodTag, fallbackMethodTag) =
      annot(MethodTagAT).map(tagSpec).getOrElse((NothingTpe, FallbackTag.Empty))
    val (baseParamTag, fallbackParamTag) =
      annot(ParamTagAT).map(tagSpec).getOrElse((NothingTpe, FallbackTag.Empty))

    lazy val rawMethods: List[RawMethod] =
      tpe.members.sorted.iterator.filter(m => m.isTerm && m.isAbstract).map(RawMethod(this, _)).toList
  }

  case class RealRpcTrait(tpe: Type) extends RpcTrait(tpe.typeSymbol) with RealRpcSymbol {
    def shortDescription = "real RPC"
    def description = s"$shortDescription $tpe"

    lazy val realMethods: List[RealMethod] =
      tpe.members.sorted.iterator.filter(m => m.isTerm && m.isAbstract).map(RealMethod(this, _)).toList
  }
}
