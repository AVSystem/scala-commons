package com.avsystem.commons
package macros.misc

import com.avsystem.commons.macros.rpc.{Fail, Ok, Res}

import scala.annotation.StaticAnnotation
import scala.reflect.{ClassTag, classTag}

trait MacroMetadatas extends MacroSymbols {

  import c.universe._

  val ParamPositionObj: Tree = q"$RpcPackage.ParamPosition"
  val TypedMetadataType: Type = getType(tq"$RpcPackage.TypedMetadata[_]")
  val MetadataParamStrategyType: Type = getType(tq"$RpcPackage.MetadataParamStrategy")
  val ReifyAnnotAT: Type = getType(tq"$RpcPackage.reifyAnnot")
  val IsAnnotatedAT: Type = getType(tq"$RpcPackage.isAnnotated[_]")
  val InferAT: Type = getType(tq"$RpcPackage.infer")
  val ReifyNameAT: Type = getType(tq"$RpcPackage.reifyName")
  val ReifyPositionAT: Type = getType(tq"$RpcPackage.reifyPosition")
  val ReifyFlagsAT: Type = getType(tq"$RpcPackage.reifyFlags")
  val CheckedAT: Type = getType(tq"$RpcPackage.checked")
  val ParamPositionTpe: Type = getType(tq"$RpcPackage.ParamPosition")
  val ParamFlagsTpe: Type = getType(tq"$RpcPackage.ParamFlags")

  def actualMetadataType(baseMetadataType: Type, realType: Type, realTypeDesc: String, verbatim: Boolean): Res[Type] = {
    val (wildcards, underlying) = baseMetadataType match {
      case ExistentialType(wc, u) if !verbatim => (wc, u)
      case t => (Nil, t)
    }
    val baseMethodResultType = underlying.baseType(TypedMetadataType.typeSymbol).typeArgs.head
    val result = if (wildcards.isEmpty)
      Some(baseMetadataType).filter(_ => baseMethodResultType =:= realType)
    else determineTypeParams(baseMethodResultType, realType, wildcards)
      .map(typeArgs => underlying.substituteTypes(wildcards, typeArgs))

    result.map(Ok(_)).getOrElse(Fail(
      s"$realTypeDesc $realType is incompatible with required metadata type $baseMetadataType"))
  }

  abstract class MetadataParam(val owner: MetadataConstructor, val symbol: Symbol) extends MacroParam {
    def shortDescription = "metadata parameter"
    def description = s"$shortDescription $nameStr of ${owner.description}"
  }

  class CompositeParam(owner: MetadataConstructor, symbol: Symbol) extends MetadataParam(owner, symbol) {
    val constructor: MetadataConstructor = owner.compositeConstructor(this)
    override def description: String = s"${super.description} at ${owner.description}"
  }

  type MetadataConstructor <: BaseMetadataConstructor

  abstract class BaseMetadataConstructor(constructor: Symbol) extends MacroMethod { this: MetadataConstructor =>
    def symbol: Symbol = constructor
    def ownerType: Type

    // fallback to annotations on the class itself
    def annot(tpe: Type): Option[Annot] =
      findAnnotation(constructor, tpe) orElse findAnnotation(ownerType.typeSymbol, tpe)

    def shortDescription = "metadata class"
    def description = s"$shortDescription $ownerType"

    def paramByStrategy(paramSym: Symbol, annot: Annot): MetadataParam = annot.tpe match {
      case t if t <:< InferAT => new ImplicitParam(this, paramSym)
      case t if t <:< ReifyAnnotAT => new ReifiedAnnotParam(this, paramSym)
      case t if t <:< ReifyNameAT =>
        val useRpcName = annot.findArg[Boolean](ReifyNameAT.member(TermName("rpcName")), false)
        new ReifiedNameParam(this, paramSym, useRpcName)
      case t if t <:< IsAnnotatedAT =>
        new IsAnnotatedParam(this, paramSym, t.typeArgs.head)
      case t => reportProblem(s"metadata param strategy $t is not allowed here")
    }

    def compositeConstructor(param: CompositeParam): MetadataConstructor

    lazy val paramLists: List[List[MetadataParam]] =
      constructor.typeSignatureIn(ownerType).paramLists.map(_.map { ps =>
        if (findAnnotation(ps, CompositeAT).nonEmpty)
          new CompositeParam(this, ps)
        else findAnnotation(ps, MetadataParamStrategyType).map(paramByStrategy(ps, _)).getOrElse {
          if (ps.isImplicit) new ImplicitParam(this, ps)
          else reportProblem("no metadata param strategy annotation found")
        }
      })

    def constructorCall(argDecls: List[Tree]): Tree =
      q"""
        ..$argDecls
        new $ownerType(...$argLists)
      """

    protected def cast[C <: MetadataConstructor : ClassTag]: C = this match {
      case c: C => c
      case _ => throw new Exception(s"Metadata constructor $this is not a ${classTag[C].runtimeClass.getSimpleName}")
    }
  }

  sealed abstract class DirectMetadataParam(owner: MetadataConstructor, symbol: Symbol)
    extends MetadataParam(owner, symbol) {

    def materializeFor(matchedSymbol: MatchedSymbol): Tree
    def tryMaterializeFor(matchedSymbol: MatchedSymbol): Res[Tree]
  }

  class ImplicitParam(owner: MetadataConstructor, symbol: Symbol)
    extends DirectMetadataParam(owner, symbol) {

    val checked: Boolean = findAnnotation(symbol, CheckedAT).nonEmpty

    def materializeFor(matchedSymbol: MatchedSymbol): Tree =
      q"${infer(actualType)}"

    def tryMaterializeFor(matchedSymbol: MatchedSymbol): Res[Tree] =
      if (checked)
        tryInferCachedImplicit(actualType).map(n => Ok(q"$n"))
          .getOrElse(Fail(s"no implicit value $actualType for $description could be found"))
      else
        Ok(materializeFor(matchedSymbol))
  }

  class ReifiedAnnotParam(owner: MetadataConstructor, symbol: Symbol)
    extends DirectMetadataParam(owner, symbol) with ArityParam {

    def allowMulti: Boolean = true
    def allowNamedMulti: Boolean = false
    def allowListedMulti: Boolean = true

    if (!(arity.collectedType <:< typeOf[StaticAnnotation])) {
      reportProblem(s"${arity.collectedType} is not a subtype of StaticAnnotation")
    }

    def materializeFor(matchedSymbol: MatchedSymbol): Tree = {
      def validated(annot: Annot): Annot = {
        if (containsInaccessibleThises(annot.tree)) {
          echo(showCode(annot.tree))
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

    def tryMaterializeFor(matchedSymbol: MatchedSymbol): Res[Tree] =
      Ok(materializeFor(matchedSymbol))
  }

  class IsAnnotatedParam(owner: MetadataConstructor, symbol: Symbol, annotTpe: Type)
    extends DirectMetadataParam(owner, symbol) {

    if (!(actualType =:= typeOf[Boolean])) {
      reportProblem("@hasAnnot can only be used on Boolean parameters")
    }

    def materializeFor(matchedSymbol: MatchedSymbol): Tree =
      q"${matchedSymbol.allAnnots(annotTpe).nonEmpty}"
    def tryMaterializeFor(matchedSymbol: MatchedSymbol): Res[Tree] =
      Ok(materializeFor(matchedSymbol))
  }

  class ReifiedNameParam(owner: MetadataConstructor, symbol: Symbol, useRpcName: Boolean)
    extends DirectMetadataParam(owner, symbol) {

    if (!(actualType =:= typeOf[String])) {
      reportProblem(s"its type is not String")
    }

    def materializeFor(matchedSymbol: MatchedSymbol): Tree =
      q"${if (useRpcName) matchedSymbol.rawName else matchedSymbol.real.nameStr}"

    def tryMaterializeFor(matchedSymbol: MatchedSymbol): Res[Tree] =
      Ok(materializeFor(matchedSymbol))
  }

  class ReifiedPositionParam(owner: MetadataConstructor, symbol: Symbol)
    extends DirectMetadataParam(owner, symbol) {

    if (!(actualType =:= ParamPositionTpe)) {
      reportProblem("its type is not ParamPosition")
    }

    def materializeFor(matchedParam: MatchedSymbol): Tree = {
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

    def tryMaterializeFor(matchedParam: MatchedSymbol): Res[Tree] =
      Ok(materializeFor(matchedParam))
  }

  class ReifiedFlagsParam(owner: MetadataConstructor, symbol: Symbol)
    extends DirectMetadataParam(owner, symbol) {

    if (!(actualType =:= ParamFlagsTpe)) {
      reportProblem("its type is not ParamFlags")
    }

    def materializeFor(matchedParam: MatchedSymbol): Tree = {
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

    def tryMaterializeFor(matchedParam: MatchedSymbol): Res[Tree] =
      Ok(materializeFor(matchedParam))
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
