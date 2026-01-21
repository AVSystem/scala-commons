package com.avsystem.commons
package macros.meta

import com.avsystem.commons.macros.misc.{FailMsg, Ok, Res}

import scala.annotation.{tailrec, StaticAnnotation}

private[commons] trait MacroMetadatas extends MacroSymbols {

  import c.universe.*

  final def ParamPositionObj: Tree = q"$MetaPackage.ParamPosition"
  final def MethodPositionObj: Tree = q"$MetaPackage.MethodPosition"
  final lazy val TypedMetadataType: Type = staticType(tq"$MetaPackage.TypedMetadata[_]")
  final lazy val MetadataParamStrategyType: Type = staticType(tq"$MetaPackage.MetadataParamStrategy")
  final lazy val DirectMetadataParamStrategyType: Type = staticType(tq"$MetaPackage.DirectMetadataParamStrategy")
  final lazy val ReifyAnnotAT: Type = staticType(tq"$MetaPackage.reifyAnnot")
  final lazy val IsAnnotatedAT: Type = staticType(tq"$MetaPackage.isAnnotated[_]")
  final lazy val ReifyNameAT: Type = staticType(tq"$MetaPackage.reifyName")
  final lazy val ReifyPositionAT: Type = staticType(tq"$MetaPackage.reifyPosition")
  final lazy val ReifyParamListCountAT: Type = staticType(tq"$MetaPackage.reifyParamListCount")
  final lazy val ReifyFlagsAT: Type = staticType(tq"$MetaPackage.reifyFlags")
  final lazy val CheckedAT: Type = staticType(tq"$MetaPackage.checked")
  final lazy val AllowIncompleteAT: Type = staticType(tq"$MetaPackage.allowIncomplete")
  final lazy val SymbolSourceTpe: Type = staticType(tq"$MetaPackage.SymbolSource")
  final lazy val ParamPositionTpe: Type = staticType(tq"$MetaPackage.ParamPosition")
  final lazy val ParamFlagsTpe: Type = staticType(tq"$MetaPackage.ParamFlags")
  final lazy val MethodFlagsTpe: Type = staticType(tq"$MetaPackage.MethodFlags")
  final lazy val TypeFlagsTpe: Type = staticType(tq"$MetaPackage.TypeFlags")
  final lazy val ForTypeParamsAT: Type = staticType(tq"$RpcPackage.forTypeParams")

  def actualMetadataType(baseMetadataType: Type, realType: Type, realTypeDesc: String, verbatim: Boolean): Res[Type] = {
    val (wildcards, underlying) = baseMetadataType match {
      case ExistentialType(wc, u) if !verbatim => (wc, u)
      case t => (Nil, t)
    }
    val asTypedMetadata = underlying.baseType(TypedMetadataType.typeSymbol)
    if (asTypedMetadata == NoType) {
      abort(s"$baseMetadataType is not a subtype of TypedMetadata")
    }
    val baseMethodResultType = asTypedMetadata.typeArgs.head
    val result =
      if (wildcards.isEmpty)
        Some(baseMetadataType).filter(_ => baseMethodResultType =:= realType)
      else
        determineTypeParams(baseMethodResultType, realType, wildcards)
          .map(typeArgs => underlying.substituteTypes(wildcards, typeArgs))

    result
      .map(Ok(_))
      .getOrElse(FailMsg(s"$realTypeDesc $realType is incompatible with required metadata type $baseMetadataType"))
  }

  def materializeOneOf(mdType: Type)(materialize: Type => Res[Tree]): Res[Tree] =
    knownSubtypes(mdType, ordered = true) match {
      case Some(subtypes) =>
        Res.firstOk(subtypes)(materialize) { errorsByType =>
          s"none of the case types of $mdType could be materialized:\n" +
            errorsByType.iterator
              .map { case (st, err) =>
                s" * $st failed because: ${indent(err, " ")}"
              }
              .mkString("\n")
        }
      case None => materialize(mdType)
    }

  case class TypeParamInstances(paramType: Type, param: MetadataParam) {
    val typeClass: Symbol => Type = paramType.baseType(BIterableClass) match {
      case TypeRef(_, _, List(ExistentialType(List(q), TypeRef(pre, sym, List(wc)))))
          if wc =:= internal.typeRef(NoPrefix, q, Nil) =>
        targ => internal.typeRef(pre, sym, List(internal.typeRef(NoPrefix, targ, Nil)))
      case _ =>
        param.reportProblem(s"bad input type of @forTypeParams parameter: $paramType")
    }

    def withInstances(tree: Tree, typeParams: List[MacroTypeParam]): Tree = {
      val funParamName = c.freshName(TermName("instances"))
      val itName = c.freshName(TermName("it"))
      val itDecl = q"val $itName = $funParamName.iterator"
      val statements =
        if (typeParams.isEmpty) Nil
        else
          itDecl :: typeParams.map { tp =>
            tp.validate()
            q"val ${tp.instanceName} = $RpcPackage.RpcMetadata.nextInstance[${typeClass(tp.symbol)}]($itName, ${tp.description})"
          }
      q"($funParamName: $paramType) => {..$statements; $tree}"
    }
  }

  abstract class MetadataParam(val ownerConstr: MetadataConstructor, val symbol: Symbol)
    extends MacroParam with TagSpecifyingSymbol {

    def tagSpecifyingOwner: Option[TagSpecifyingSymbol] = Some(ownerConstr)

    def seenFrom: Type = ownerConstr.constructed
    def shortDescription = "metadata parameter"
    def description = s"$shortDescription $nameStr of ${ownerConstr.description}"
    def pathStr: String = ownerConstr.ownerParam.fold(nameStr)(cp => s"${cp.pathStr}.$nameStr")

    val (typeParamInstances, typeGivenInstances) =
      if (annot(ForTypeParamsAT).isEmpty) (None, collectedType)
      else
        collectedType.baseType(definitions.FunctionClass(1)) match {
          case TypeRef(_, _, List(in, out)) =>
            (Some(TypeParamInstances(in, this)), out)
          case _ =>
            reportProblem(s"the type of a @forTypeParams parameter must be a function")
        }

    def typeParamTypeClassInContext: Option[Symbol => Type] =
      typeParamInstances.map(_.typeClass) orElse ownerConstr.ownerParam.flatMap(_.typeParamTypeClassInContext)

    def withTypeParamInstances(typeParamsInContext: List[MacroTypeParam])(tree: Tree): Tree =
      typeParamInstances.fold(tree)(_.withInstances(tree, typeParamsInContext))
  }

  class CompositeParam(owner: MetadataConstructor, symbol: Symbol)
    extends MetadataParam(owner, symbol) with ArityParam {

    def allowSingle: Boolean = true
    def allowOptional: Boolean = true
    def allowNamedMulti: Boolean = false
    def allowListedMulti: Boolean = false

    val constructor: MetadataConstructor = owner.compositeConstructor(this)
    override def description: String = s"${super.description} at ${owner.description}"

    def tryMaterialize(symbol: MatchedSymbol)(paramMaterialize: MetadataParam => Res[Tree]): Res[Tree] = {
      val withoutArity = for {
        newMatched <- constructor.matchTagsAndFilters(symbol)
        tree <- constructor.tryMaterialize(newMatched)(paramMaterialize)
      } yield tree
      val res = arity match {
        case _: ParamArity.Single => withoutArity
        case _: ParamArity.Optional => Ok(mkOptional(withoutArity.toOption))
        case _ => FailMsg(s"${arity.annotStr} not allowed for @composite params")
      }
      res.map(withTypeParamInstances(symbol.typeParamsInContext))
    }
  }

  abstract class MetadataConstructor(val constructed: Type, val ownerParam: Option[MetadataParam])
    extends MacroTypeSymbol with TagMatchingSymbol with TagSpecifyingSymbol {

    def seenFrom: Type = constructed
    def tagSpecifyingOwner: Option[TagSpecifyingSymbol] = ownerParam

    lazy val symbol: Symbol =
      constructed.typeSymbol

    lazy val sig: Type =
      primaryConstructor(constructed, ownerParam).typeSignatureIn(constructed)

    lazy val allowIncomplete: Boolean =
      annot(AllowIncompleteAT).nonEmpty

    def shortDescription = "metadata class"
    def description = s"$shortDescription $constructed"

    def paramByStrategy(paramSym: Symbol, annot: Annot, ownerConstr: MetadataConstructor = this): MetadataParam =
      annot.tpe match {
        case t if t <:< InferAT =>
          val clue = annot.findArg[String](InferAT.member(TermName("clue")), "")
          new ImplicitParam(ownerConstr, paramSym, clue)
        case t if t <:< ReifyAnnotAT =>
          new ReifiedAnnotParam(ownerConstr, paramSym)
        case t if t <:< ReifyNameAT =>
          val useRawName = annot.findArg[Boolean](ReifyNameAT.member(TermName("useRawName")), false)
          new ReifiedNameParam(ownerConstr, paramSym, useRawName)
        case t if t <:< IsAnnotatedAT =>
          new IsAnnotatedParam(ownerConstr, paramSym, t.typeArgs.head)
        case t =>
          reportProblem(s"metadata param strategy $t is not allowed here")
      }

    def compositeConstructor(param: CompositeParam): MetadataConstructor

    lazy val paramLists: List[List[MetadataParam]] =
      sig.paramLists.map(_.map { ps =>
        if (findAnnotation(ps, CompositeAT).nonEmpty)
          new CompositeParam(this, ps)
        else
          findAnnotation(ps, MetadataParamStrategyType, constructed).map(paramByStrategy(ps, _)).getOrElse {
            if (ps.isImplicit) new ImplicitParam(this, ps, "")
            else new InvalidParam(this, ps, "no metadata param strategy annotation found")
          }
      })

    protected def collectParams[P <: MetadataParam: ClassTag]: List[P] =
      paramLists.flatten.flatMap {
        case cp: CompositeParam => cp.constructor.collectParams[P]
        case p: P => List(p)
        case _ => Nil
      }

    def argLists: List[List[Tree]] =
      paramLists.map(_.map(_.argToPass))

    def constructorCall(argDecls: List[Tree]): Tree =
      q"""
        ..$argDecls
        new $constructed(...$argLists)
      """

    protected def cast[C <: MetadataConstructor: ClassTag]: C = this match {
      case constructor: C => constructor
      case _ => throw new Exception(s"Metadata constructor $this is not a ${classTag[C].runtimeClass.getSimpleName}")
    }

    def tryMaterialize(symbol: MatchedSymbol)(paramMaterialize: MetadataParam => Res[Tree]): Res[Tree] =
      Res
        .traverse(paramLists.flatten) { mp =>
          val res = mp match {
            case cp: CompositeParam => cp.tryMaterialize(symbol)(paramMaterialize)
            case dmp: DirectMetadataParam => dmp.tryMaterializeFor(symbol)
            case _ => paramMaterialize(mp)
          }
          res.map(mp.localValueDecl)
        }
        .map(constructorCall)
  }

  abstract class DirectMetadataParam(owner: MetadataConstructor, symbol: Symbol) extends MetadataParam(owner, symbol) {

    def tryMaterializeFor(matchedSymbol: MatchedSymbol): Res[Tree]
  }

  class ImplicitParam(owner: MetadataConstructor, symbol: Symbol, clue: String)
    extends DirectMetadataParam(owner, symbol) with ArityParam {

    def allowSingle: Boolean = true
    def allowOptional: Boolean = true
    def allowNamedMulti: Boolean = false
    def allowListedMulti: Boolean = false

    val checked: Boolean = annot(CheckedAT).nonEmpty

    def tryMaterializeFor(matchedSymbol: MatchedSymbol): Res[Tree] = {
      val tpe = typeGivenInstances
      val tpTypeClass = typeParamTypeClassInContext
      val allTparams = if (tpTypeClass.isEmpty) Nil else matchedSymbol.typeParamsInContext
      val usedTparams = allTparams.filter(tp => tpe.contains(tp.symbol))
      val tparamSymbols = usedTparams.map(_.symbol)
      val tparamInstanceTypes = tpTypeClass.fold(List.empty[Type])(tc => tparamSymbols.map(tc))

      def referImplicit(ci: CachedImplicit): Tree =
        withTypeParamInstances(allTparams)(ci.reference(usedTparams.map(tp => q"${tp.instanceName}")))

      arity match {
        case ParamArity.Single(_) =>
          if (checked)
            tryInferCachedImplicit(tpe, tparamSymbols, tparamInstanceTypes, expandMacros = true)
              .map(ci => Ok(referImplicit(ci)))
              .getOrElse(FailMsg(clue + implicitNotFoundMsg(tpe)))
          else
            Ok(referImplicit(infer(tpe, tparamSymbols, tparamInstanceTypes, matchedSymbol.real, clue)))
        case ParamArity.Optional(_) =>
          Ok(
            mkOptional(
              tryInferCachedImplicit(tpe, tparamSymbols, tparamInstanceTypes, expandMacros = true).map(referImplicit),
            ),
          )
        case _ =>
          FailMsg(s"${arity.annotStr} not allowed on @infer params")
      }
    }
  }

  class ReifiedAnnotConstructor(annotTpe: Type, reifiedAnnotParam: ReifiedAnnotParam)
    extends MetadataConstructor(annotTpe, Some(reifiedAnnotParam)) {

    def baseTagSpecs: List[BaseTagSpec] = reifiedAnnotParam.ownerConstr.baseTagSpecs

    def compositeConstructor(param: CompositeParam): MetadataConstructor =
      param.ownerConstr.compositeConstructor(param)

    override def paramByStrategy(paramSym: Symbol, annot: Annot, ownerConstr: MetadataConstructor): MetadataParam =
      reifiedAnnotParam.ownerConstr.paramByStrategy(paramSym, annot, ownerConstr)
  }

  class ReifiedAnnotParam(owner: MetadataConstructor, symbol: Symbol)
    extends DirectMetadataParam(owner, symbol) with ArityParam {

    def allowSingle: Boolean = true
    def allowOptional: Boolean = true
    def allowNamedMulti: Boolean = false
    def allowListedMulti: Boolean = true

    if (!(typeGivenInstances <:< typeOf[StaticAnnotation])) {
      reportProblem(s"$typeGivenInstances is not a subtype of StaticAnnotation")
    }

    def tryMaterializeFor(matchedSymbol: MatchedSymbol): Res[Tree] = {
      val annotTpe = typeGivenInstances
      val annotConstr = new ReifiedAnnotConstructor(annotTpe, this)
      val tparams = matchedSymbol.typeParamsInContext

      def materializeAnnotParam(param: Symbol): Option[Res[Tree]] =
        if (findAnnotation(param, CompositeAT).nonEmpty)
          Some(
            new CompositeParam(annotConstr, param)
              .tryMaterialize(matchedSymbol)(p => FailMsg(s"unexpected metadata parameter $p")),
          )
        else
          findAnnotation(param, DirectMetadataParamStrategyType)
            .map(annot => annotConstr.paramByStrategy(param, annot))
            .map {
              case dmp: DirectMetadataParam => dmp.tryMaterializeFor(matchedSymbol)
              case p => FailMsg(s"unexpected metadata parameter $p")
            }

      def resolvedAnnotTree(annot: Annot): Res[Tree] =
        annot.treeRes.map { tree =>
          if (containsInaccessibleThises(tree)) {
            matchedSymbol.real.reportProblem(s"reified annotation $tree contains inaccessible this-references")
          }

          withTypeParamInstances(tparams)(c.untypecheck(tree))
        }

      arity match {
        case ParamArity.Single(_) =>
          matchedSymbol
            .annot(annotTpe, materializeAnnotParam)
            .map(resolvedAnnotTree)
            .getOrElse(FailMsg(s"no annotation of type $annotTpe found"))
        case ParamArity.Optional(_) =>
          Res
            .sequence(matchedSymbol.annot(annotTpe, materializeAnnotParam).map(resolvedAnnotTree))
            .map(ot => mkOptional(ot))
        case ParamArity.Multi(_, _) =>
          Res
            .sequence(matchedSymbol.annots(annotTpe, materializeAnnotParam).map(resolvedAnnotTree))
            .map(tl => mkMulti(tl))
        case _ =>
          FailMsg(s"${arity.annotStr} not allowed on @reifyAnnot params")
      }
    }
  }

  class IsAnnotatedParam(owner: MetadataConstructor, symbol: Symbol, annotTpe: Type)
    extends DirectMetadataParam(owner, symbol) {

    if (!(actualType =:= typeOf[Boolean])) {
      reportProblem("@hasAnnot can only be used on Boolean parameters")
    }

    def tryMaterializeFor(matchedSymbol: MatchedSymbol): Res[Tree] =
      Ok(q"${matchedSymbol.annots(annotTpe).nonEmpty}")
  }

  class ReifiedNameParam(owner: MetadataConstructor, symbol: Symbol, useRawName: Boolean)
    extends DirectMetadataParam(owner, symbol) {

    if (!(actualType =:= typeOf[String])) {
      reportProblem(s"its type is not String")
    }

    def tryMaterializeFor(matchedSymbol: MatchedSymbol): Res[Tree] =
      Ok(q"${if (useRawName) matchedSymbol.rawName else matchedSymbol.real.nameStr}")
  }

  class ParamPositionParam(owner: MetadataConstructor, symbol: Symbol) extends DirectMetadataParam(owner, symbol) {

    if (!(actualType =:= ParamPositionTpe)) {
      reportProblem("its type is not ParamPosition")
    }

    def tryMaterializeFor(matchedParam: MatchedSymbol): Res[Tree] = Ok {
      val paramSym = matchedParam.real.symbol
      @tailrec def loop(index: Int, indexInList: Int, indexOfList: Int, paramLists: List[List[Symbol]]): Tree =
        paramLists match {
          case (sym :: _) :: _ if sym.name == paramSym.name =>
            q"$ParamPositionObj($index, $indexOfList, $indexInList, ${matchedParam.indexInRaw})"
          case (_ :: rest) :: tail =>
            loop(index + 1, indexInList + 1, indexOfList, rest :: tail)
          case Nil :: rest =>
            loop(index, 0, indexOfList + 1, rest)
          case Nil =>
            abort(s"$paramSym not found in its owner param lists")
        }
      loop(0, 0, 0, paramSym.owner.typeSignature.paramLists)
    }
  }

  class ParamFlagsParam(owner: MetadataConstructor, symbol: Symbol) extends DirectMetadataParam(owner, symbol) {

    if (!(actualType =:= ParamFlagsTpe)) {
      reportProblem("its type is not ParamFlags")
    }

    def tryMaterializeFor(matchedParam: MatchedSymbol): Res[Tree] = Ok {
      val rpcSym = matchedParam.real
      def flag(cond: Boolean, bit: Int) = if (cond) 1 << bit else 0
      val s = rpcSym.symbol.asTerm
      val rawFlags =
        flag(s.isImplicit, 0) | flag(s.isByNameParam, 1) | flag(isRepeated(s), 2) | flag(s.isParamWithDefault, 3) |
          flag(s.isSynthetic, 4)
      q"new $ParamFlagsTpe($rawFlags)"
    }
  }

  class InvalidParam(owner: MetadataConstructor, symbol: Symbol, problem: String) extends MetadataParam(owner, symbol) {
    reportProblem(problem)
  }

  def guardedMetadata(metadataTpe: Type, realTpe: Type)(materialize: => Tree): Tree = {
    // separate object for cached implicits so that lazy vals are members instead of local variables
    val depsObj = c.freshName(TermName("deps"))
    val selfName = c.freshName(TermName("self"))

    typedCompanionOf(metadataTpe) match {
      case Some(comp) =>
        // short circuit recursive implicit searches for M.Lazy[Real]
        val lazyMetadataTpe = getType(tq"$comp.Lazy[$realTpe]")
        val lazySelfName = c.freshName(TermName("lazySelf"))
        registerImplicit(lazyMetadataTpe, lazySelfName)
        val tree = materialize

        q"""
          object $depsObj {
            var $selfName: $metadataTpe = _
            private val $lazySelfName = $comp.Lazy($selfName)
            ..$cachedImplicitDeclarations
            $selfName = $tree
          }
          $depsObj.$selfName
         """

      case None =>
        val tree = materialize
        q"""
          object $depsObj {
            ..$cachedImplicitDeclarations
            val $selfName = $tree
          }
          $depsObj.$selfName
         """
    }
  }
}
