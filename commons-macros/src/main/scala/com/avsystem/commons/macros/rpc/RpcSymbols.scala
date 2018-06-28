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
      val at = param.annot(RpcArityAT).fold(SingleArityAT)(_.tpe)
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
      annot(RpcNameAT).fold(nameStr)(_.findArg[String](RpcNameNameSym))
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

  trait AritySymbol extends RpcSymbol {
    val arity: RpcArity

    // @unchecked because "The outer reference in this type test cannot be checked at runtime"
    // Srsly scalac, from static types it should be obvious that outer references are the same
    def matchName(realRpcSymbol: RealRpcSymbol): Res[Unit] = arity match {
      case _: RpcArity.Single@unchecked | _: RpcArity.Optional@unchecked =>
        if (realRpcSymbol.rpcName == nameStr) Ok(())
        else Fail(s"it only matches ${realRpcSymbol.shortDescription}s with RPC name $nameStr")
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

    def mkMulti[T: Liftable](elements: List[T]): Tree = {
      val builderName = c.freshName(TermName("builder"))
      q"""
        val $builderName = $canBuildFrom()
        $builderName.sizeHint(${elements.size})
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

    def baseTag: Type = containingRawMethod.baseParamTag
    def defaultTag: Type = containingRawMethod.defaultParamTag

    def cannotMapClue = s"cannot map it to $shortDescription $pathStr of ${containingRawMethod.nameStr}"
  }

  case class RealParam(owner: RealMethod, symbol: Symbol, index: Int, indexOfList: Int, indexInList: Int)
    extends RpcParam with RealRpcSymbol {

    def shortDescription = "real parameter"
    def description = s"$shortDescription $nameStr of ${owner.description}"

    val whenAbsent: Tree = annot(WhenAbsentAT).fold(EmptyTree) { annot =>
      val annotatedDefault = annot.tree.children.tail.head
      if (!(annotatedDefault.tpe <:< actualType)) {
        reportProblem(s"expected value of type $actualType in @whenAbsent annotation, got ${annotatedDefault.tpe.widen}")
      }
      val transformer = new Transformer {
        override def transform(tree: Tree): Tree = tree match {
          case Super(t@This(_), _) if !enclosingClasses.contains(t.symbol) =>
            reportProblem(s"illegal super-reference in @whenAbsent annotation")
          case This(_) if tree.symbol == owner.owner.symbol => q"${owner.owner.safeName}"
          case This(_) if !enclosingClasses.contains(tree.symbol) =>
            reportProblem(s"illegal this-reference in @whenAbsent annotation")
          case t => super.transform(t)
        }
      }
      transformer.transform(annotatedDefault)
    }

    def defaultValueTree: Tree =
      if (whenAbsent != EmptyTree) c.untypecheck(whenAbsent)
      else if (symbol.asTerm.isParamWithDefault) {
        val prevListParams = owner.realParams.take(index - indexInList).map(rp => q"${rp.safeName}")
        val prevListParamss = List(prevListParams).filter(_.nonEmpty)
        q"${owner.owner.safeName}.${TermName(s"${owner.encodedNameStr}$$default$$${index + 1}")}(...$prevListParamss)"
      }
      else q"$RpcPackage.RpcUtils.missingArg(${owner.rpcName}, $rpcName)"
  }

  case class RawMethod(owner: RawRpcTrait, symbol: Symbol) extends RpcMethod with RawRpcSymbol with AritySymbol {
    def shortDescription = "raw method"
    def description = s"$shortDescription $nameStr of ${owner.description}"

    def ownerType: Type = owner.tpe
    def baseTag: Type = owner.baseMethodTag
    def defaultTag: Type = owner.defaultMethodTag

    val arity: RpcMethodArity = RpcMethodArity.fromAnnotation(this)

    val verbatimResult: Boolean =
      annot(RpcEncodingAT).map(_.tpe <:< VerbatimAT).getOrElse(arity.verbatimByDefault)

    val List(baseParamTag, defaultParamTag) =
      annot(ParamTagAT)
        .map(_.tpe.baseType(ParamTagAT.typeSymbol).typeArgs)
        .getOrElse(List(owner.baseParamTag, owner.defaultParamTag))

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
          case Nil => q"$RpcPackage.RpcUtils.missingOptionalRpc($nameStr)"
          case List((_, single)) => single
          case _ => abort(s"multiple real methods match $description")
        }
        case RpcMethodArity.Multi =>
          val methodNameName = c.freshName(TermName("methodName"))
          q"""
            ${methodNameParam.safePath} match {
              case ..${caseDefs.map({ case (rpcName, tree) => cq"$rpcName => $tree" })}
              case $methodNameName => $RpcPackage.RpcUtils.unknownRpc($methodNameName, $nameStr)
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
