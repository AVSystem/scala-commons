package com.avsystem.commons
package macros.meta

import com.avsystem.commons.macros.misc.{Fail, Ok, Res}

import scala.annotation.StaticAnnotation
import scala.reflect.{ClassTag, classTag}

trait MacroMetadatas extends MacroSymbols {

  import c.universe._

  val ParamPositionObj: Tree = q"$MetaPackage.ParamPosition"
  val TypedMetadataType: Type = getType(tq"$MetaPackage.TypedMetadata[_]")
  val MetadataParamStrategyType: Type = getType(tq"$MetaPackage.MetadataParamStrategy")
  val ReifyAnnotAT: Type = getType(tq"$MetaPackage.reifyAnnot")
  val IsAnnotatedAT: Type = getType(tq"$MetaPackage.isAnnotated[_]")
  val InferAT: Type = getType(tq"$MetaPackage.infer")
  val ReifyNameAT: Type = getType(tq"$MetaPackage.reifyName")
  val ReifyPositionAT: Type = getType(tq"$MetaPackage.reifyPosition")
  val ReifyFlagsAT: Type = getType(tq"$MetaPackage.reifyFlags")
  val CheckedAT: Type = getType(tq"$MetaPackage.checked")
  val ParamPositionTpe: Type = getType(tq"$MetaPackage.ParamPosition")
  val ParamFlagsTpe: Type = getType(tq"$MetaPackage.ParamFlags")
  val TypeFlagsTpe: Type = getType(tq"$MetaPackage.TypeFlags")

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
    val result = if (wildcards.isEmpty)
      Some(baseMetadataType).filter(_ => baseMethodResultType =:= realType)
    else determineTypeParams(baseMethodResultType, realType, wildcards)
      .map(typeArgs => underlying.substituteTypes(wildcards, typeArgs))

    result.map(Ok(_)).getOrElse(Fail(
      s"$realTypeDesc $realType is incompatible with required metadata type $baseMetadataType"))
  }

  def materializeOneOf(mdType: Type)(materialize: Type => Res[Tree]): Res[Tree] =
    knownSubtypes(mdType, ordered = true) match {
      case Some(subtypes) => Res.firstOk(subtypes)(materialize) { errorsByType =>
        s"none of the case types of $mdType could be materialized:\n" +
          errorsByType.iterator.map {
            case (st, err) => s" * $st failed because: ${indent(err, " ")}"
          }.mkString("\n")
      }
      case None => materialize(mdType)
    }

  abstract class MetadataParam(val owner: MetadataConstructor, val symbol: Symbol) extends MacroParam {
    def shortDescription = "metadata parameter"
    def description = s"$shortDescription $nameStr of ${owner.description}"
    def pathStr: String = owner.atParam.fold(nameStr)(cp => s"${cp.pathStr}.$nameStr")
  }

  class CompositeParam(owner: MetadataConstructor, symbol: Symbol) extends MetadataParam(owner, symbol) {
    val constructor: MetadataConstructor = owner.compositeConstructor(this)
    override def description: String = s"${super.description} at ${owner.description}"
  }

  abstract class MetadataConstructor(val ownerType: Type, val atParam: Option[CompositeParam])
    extends MacroMethod { this: MetadataConstructor =>

    lazy val symbol: Symbol = primaryConstructor(ownerType, atParam)

    // fallback to annotations on the class itself
    def annot(tpe: Type): Option[Annot] =
      findAnnotation(symbol, tpe) orElse findAnnotation(ownerType.typeSymbol, tpe)

    def shortDescription = "metadata class"
    def description = s"$shortDescription $ownerType"

    def paramByStrategy(paramSym: Symbol, annot: Annot): MetadataParam = annot.tpe match {
      case t if t <:< InferAT => new ImplicitParam(this, paramSym)
      case t if t <:< ReifyAnnotAT => new ReifiedAnnotParam(this, paramSym)
      case t if t <:< ReifyNameAT =>
        val useRawName = annot.findArg[Boolean](ReifyNameAT.member(TermName("useRawName")), false)
        new ReifiedNameParam(this, paramSym, useRawName)
      case t if t <:< IsAnnotatedAT =>
        new IsAnnotatedParam(this, paramSym, t.typeArgs.head)
      case t => reportProblem(s"metadata param strategy $t is not allowed here")
    }

    def compositeConstructor(param: CompositeParam): MetadataConstructor

    lazy val paramLists: List[List[MetadataParam]] =
      symbol.typeSignatureIn(ownerType).paramLists.map(_.map { ps =>
        if (findAnnotation(ps, CompositeAT).nonEmpty)
          new CompositeParam(this, ps)
        else findAnnotation(ps, MetadataParamStrategyType).map(paramByStrategy(ps, _)).getOrElse {
          if (ps.isImplicit) new ImplicitParam(this, ps)
          else reportProblem("no metadata param strategy annotation found")
        }
      })

    protected def collectParams[P <: MetadataParam : ClassTag]: List[P] =
      paramLists.flatten.flatMap {
        case cp: CompositeParam => cp.constructor.collectParams[P]
        case p: P => List(p)
        case _ => Nil
      }

    def constructorCall(argDecls: List[Tree]): Tree =
      q"""
        ..$argDecls
        new $ownerType(...$argLists)
      """

    protected def cast[C <: MetadataConstructor : ClassTag]: C = this match {
      case c: C => c
      case _ => throw new Exception(s"Metadata constructor $this is not a ${classTag[C].runtimeClass.getSimpleName}")
    }

    protected def tryMaterialize(symbol: MatchedSymbol)(paramMaterialize: MetadataParam => Res[Tree]): Res[Tree] =
      Res.traverse(paramLists.flatten) { mp =>
        val res = mp match {
          case cp: CompositeParam => cp.constructor.tryMaterialize(symbol)(paramMaterialize)
          case dmp: DirectMetadataParam => dmp.tryMaterializeFor(symbol)
          case _ => paramMaterialize(mp)
        }
        res.map(mp.localValueDecl)
      }.map(constructorCall)
  }

  abstract class DirectMetadataParam(owner: MetadataConstructor, symbol: Symbol)
    extends MetadataParam(owner, symbol) {

    def tryMaterializeFor(matchedSymbol: MatchedSymbol): Res[Tree]
  }

  class ImplicitParam(owner: MetadataConstructor, symbol: Symbol)
    extends DirectMetadataParam(owner, symbol) with ArityParam {

    def allowMulti: Boolean = false
    def allowNamedMulti: Boolean = false
    def allowListedMulti: Boolean = false

    val checked: Boolean = findAnnotation(symbol, CheckedAT).nonEmpty

    def tryMaterializeFor(matchedSymbol: MatchedSymbol): Res[Tree] =
      arity match {
        case ParamArity.Single(tpe) =>
          if (checked)
            tryInferCachedImplicit(tpe).map(n => Ok(q"$n"))
              .getOrElse(Fail(s"no implicit value $tpe for $description could be found"))
          else
            Ok(q"${infer(tpe)}")
        case ParamArity.Optional(tpe) =>
          Ok(mkOptional(tryInferCachedImplicit(tpe).map(n => q"$n")))
        case _: ParamArity.Multi =>
          Fail("@multi arity not allowed on @infer params")
      }

  }

  class ReifiedAnnotParam(owner: MetadataConstructor, symbol: Symbol)
    extends DirectMetadataParam(owner, symbol) with ArityParam {

    def allowMulti: Boolean = true
    def allowNamedMulti: Boolean = false
    def allowListedMulti: Boolean = true

    if (!(arity.collectedType <:< typeOf[StaticAnnotation])) {
      reportProblem(s"${arity.collectedType} is not a subtype of StaticAnnotation")
    }

    def tryMaterializeFor(matchedSymbol: MatchedSymbol): Res[Tree] = Ok {
      def validated(annot: Annot): Annot = {
        if (containsInaccessibleThises(annot.tree)) {
          matchedSymbol.real.reportProblem(s"reified annotation contains this-references inaccessible outside RPC trait")
        }
        annot
      }

      val rpcSym = matchedSymbol.real
      arity match {
        case ParamArity.Single(annotTpe) =>
          matchedSymbol.annot(annotTpe).map(a => c.untypecheck(validated(a).tree)).getOrElse {
            val msg = s"${rpcSym.problemStr}: cannot materialize value for $description: no annotation of type $annotTpe found"
            q"$RpcUtils.compilationError(${StringLiteral(msg, rpcSym.pos)})"
          }
        case ParamArity.Optional(annotTpe) =>
          mkOptional(matchedSymbol.annot(annotTpe).map(a => c.untypecheck(validated(a).tree)))
        case ParamArity.Multi(annotTpe, _) =>
          mkMulti(allAnnotations(rpcSym.symbol, annotTpe).map(a => c.untypecheck(validated(a).tree)))
      }
    }
  }

  class IsAnnotatedParam(owner: MetadataConstructor, symbol: Symbol, annotTpe: Type)
    extends DirectMetadataParam(owner, symbol) {

    if (!(actualType =:= typeOf[Boolean])) {
      reportProblem("@hasAnnot can only be used on Boolean parameters")
    }

    def tryMaterializeFor(matchedSymbol: MatchedSymbol): Res[Tree] =
      Ok(q"${matchedSymbol.allAnnots(annotTpe).nonEmpty}")
  }

  class ReifiedNameParam(owner: MetadataConstructor, symbol: Symbol, useRawName: Boolean)
    extends DirectMetadataParam(owner, symbol) {

    if (!(actualType =:= typeOf[String])) {
      reportProblem(s"its type is not String")
    }

    def tryMaterializeFor(matchedSymbol: MatchedSymbol): Res[Tree] =
      Ok(q"${if (useRawName) matchedSymbol.rawName else matchedSymbol.real.nameStr}")
  }

  class ReifiedPositionParam(owner: MetadataConstructor, symbol: Symbol)
    extends DirectMetadataParam(owner, symbol) {

    if (!(actualType =:= ParamPositionTpe)) {
      reportProblem("its type is not ParamPosition")
    }

    def tryMaterializeFor(matchedParam: MatchedSymbol): Res[Tree] = Ok {
      val sym = matchedParam.real.symbol
      def loop(index: Int, indexInList: Int, indexOfList: Int, paramLists: List[List[Symbol]]): Tree =
        paramLists match {
          case (`sym` :: _) :: _ =>
            q"$ParamPositionObj($index, $indexOfList, $indexInList, ${matchedParam.indexInRaw})"
          case (_ :: rest) :: tail =>
            loop(index + 1, indexInList + 1, indexOfList, rest :: tail)
          case Nil :: rest =>
            loop(index, 0, indexOfList + 1, rest)
          case Nil =>
            abort(s"$sym not found in its owner param lists")
        }
      loop(0, 0, 0, sym.owner.typeSignature.paramLists)
    }
  }

  class ReifiedFlagsParam(owner: MetadataConstructor, symbol: Symbol)
    extends DirectMetadataParam(owner, symbol) {

    if (!(actualType =:= ParamFlagsTpe)) {
      reportProblem("its type is not ParamFlags")
    }

    def tryMaterializeFor(matchedParam: MatchedSymbol): Res[Tree] = Ok {
      val rpcSym = matchedParam.real
      def flag(cond: Boolean, bit: Int) = if (cond) 1 << bit else 0
      val s = rpcSym.symbol.asTerm
      val rawFlags =
        flag(s.isImplicit, 0) |
          flag(s.isByNameParam, 1) |
          flag(isRepeated(s), 2) |
          flag(s.isParamWithDefault, 3) |
          flag(s.isSynthetic, 4)
      q"new $ParamFlagsTpe($rawFlags)"
    }
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
