package com.avsystem.commons
package macros.meta

import com.avsystem.commons.macros.MacroCommons
import com.avsystem.commons.macros.misc.{Fail, Ok, Res}

import scala.collection.mutable.ListBuffer

trait MacroSymbols extends MacroCommons {

  import c.universe._

  val RpcPackage = q"$CommonsPkg.rpc"
  val MetaPackage = q"$CommonsPkg.meta"
  val RpcUtils = q"$RpcPackage.RpcUtils"
  val OptionLikeCls = tq"$MetaPackage.OptionLike"
  val CanBuildFromCls = tq"$CollectionPkg.generic.CanBuildFrom"
  val RpcArityAT: Type = getType(tq"$MetaPackage.SymbolArity")
  val SingleArityAT: Type = getType(tq"$MetaPackage.single")
  val OptionalArityAT: Type = getType(tq"$MetaPackage.optional")
  val MultiArityAT: Type = getType(tq"$MetaPackage.multi")
  val CompositeAT: Type = getType(tq"$MetaPackage.composite")
  val AuxiliaryAT: Type = getType(tq"$MetaPackage.auxiliary")
  val AnnotatedAT: Type = getType(tq"$MetaPackage.annotated[_]")
  val TaggedAT: Type = getType(tq"$RpcPackage.tagged[_]")
  val WhenUntaggedArg: Symbol = TaggedAT.member(TermName("whenUntagged"))

  def primaryConstructor(ownerType: Type, ownerParam: Option[MacroSymbol]): Symbol =
    primaryConstructorOf(ownerType, ownerParam.fold("")(p => s"${p.problemStr}: "))

  sealed abstract class Arity
  object Arity {
    trait Single extends Arity
    trait Optional extends Arity
    trait Multi extends Arity
  }

  sealed abstract class ParamArity(val verbatimByDefault: Boolean) extends Arity {
    def collectedType: Type
  }
  object ParamArity {
    def fromAnnotation(param: ArityParam, allowListedMulti: Boolean, allowNamedMulti: Boolean): ParamArity = {

      val at = findAnnotation(param.symbol, RpcArityAT).fold(SingleArityAT)(_.tpe)
      if (at <:< SingleArityAT) ParamArity.Single(param.actualType)
      else if (at <:< OptionalArityAT) {
        val optionLikeType = typeOfCachedImplicit(param.optionLike)
        val valueMember = optionLikeType.member(TypeName("Value"))
        if (valueMember.isAbstract)
          param.reportProblem("could not determine actual value of optional parameter type")
        else
          ParamArity.Optional(valueMember.typeSignatureIn(optionLikeType))
      }
      else if ((allowListedMulti || allowNamedMulti) && at <:< MultiArityAT) {
        if (allowNamedMulti && param.actualType <:< StringPFTpe)
          Multi(param.actualType.baseType(PartialFunctionClass).typeArgs(1), named = true)
        else if (allowListedMulti && param.actualType <:< BIterableTpe)
          Multi(param.actualType.baseType(BIterableClass).typeArgs.head, named = false)
        else if (allowNamedMulti && allowListedMulti)
          param.reportProblem(s"@multi ${param.shortDescription} must be a PartialFunction of String " +
            s"(for by-name mapping) or Iterable (for sequence)")
        else if (allowListedMulti)
          param.reportProblem(s"@multi ${param.shortDescription} must be an Iterable")
        else
          param.reportProblem(s"@multi ${param.shortDescription} must be a PartialFunction of String")
      }
      else param.reportProblem(s"forbidden RPC arity annotation: $at")
    }

    case class Single(collectedType: Type) extends ParamArity(true) with Arity.Single
    case class Optional(collectedType: Type) extends ParamArity(true) with Arity.Optional
    case class Multi(collectedType: Type, named: Boolean) extends ParamArity(false) with Arity.Multi
  }

  sealed abstract class MethodArity(val verbatimByDefault: Boolean) extends Arity
  object MethodArity {
    def fromAnnotation(method: MacroMethod): MethodArity = {
      val at = findAnnotation(method.symbol, RpcArityAT).fold(SingleArityAT)(_.tpe)
      if (at <:< SingleArityAT) Single
      else if (at <:< OptionalArityAT) Optional
      else if (at <:< MultiArityAT) Multi
      else method.reportProblem(s"unrecognized RPC arity annotation: $at")
    }

    case object Single extends MethodArity(true) with Arity.Single
    case object Optional extends MethodArity(true) with Arity.Optional
    case object Multi extends MethodArity(false) with Arity.Multi
  }

  abstract class MacroSymbol {
    def symbol: Symbol
    def pos: Position = symbol.pos
    def shortDescription: String
    def description: String
    def problemStr: String = s"problem with $description"

    def reportProblem(msg: String, detailPos: Position = NoPosition): Nothing =
      abortAt(s"$problemStr: $msg", if (detailPos != NoPosition) detailPos else pos)

    def infer(tpt: Tree): TermName =
      infer(getType(tpt))

    def infer(tpe: Type, forSym: MacroSymbol = this): TermName =
      inferCachedImplicit(tpe, s"${forSym.problemStr}: ", forSym.pos)

    val name: TermName = symbol.name.toTermName
    val safeName: TermName = c.freshName(symbol.name.toTermName)
    val nameStr: String = name.decodedName.toString
    val encodedNameStr: String = name.encodedName.toString

    override def equals(other: Any): Boolean = other match {
      case rpcSym: MacroSymbol => symbol == rpcSym.symbol
      case _ => false
    }
    override def hashCode: Int = symbol.hashCode
    override def toString: String = symbol.toString
  }

  abstract class MacroMethod extends MacroSymbol {
    def ownerType: Type

    if (!symbol.isMethod) {
      abortAt(s"problem with member $nameStr of type $ownerType: it must be a method (def)", pos)
    }

    val sig: Type = symbol.typeSignatureIn(ownerType)
    def paramLists: List[List[MacroParam]]
    val resultType: Type = sig.finalResultType

    def argLists: List[List[Tree]] = paramLists.map(_.map(_.argToPass))
    def paramDecls: List[List[Tree]] = paramLists.map(_.map(_.paramDecl))
  }

  abstract class MacroParam extends MacroSymbol {
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

  trait AritySymbol extends MacroSymbol {
    val arity: Arity

    // @unchecked because "The outer reference in this type test cannot be checked at runtime"
    // Srsly scalac, from static types it should be obvious that outer references are the same
    def matchName(shortDescr: String, name: String): Res[Unit] = arity match {
      case _: Arity.Single@unchecked | _: Arity.Optional@unchecked =>
        if (name == nameStr) Ok(())
        else Fail(s"it only matches ${shortDescr}s named $nameStr")
      case _: Arity.Multi@unchecked => Ok(())
    }
  }

  trait FilteringSymbol extends MacroSymbol {
    lazy val requiredAnnots: List[Type] =
      allAnnotations(symbol, AnnotatedAT).map(_.tpe.dealias.typeArgs.head)

    def matchFilters(realSymbol: MatchedSymbol): Res[Unit] =
      Res.traverse(requiredAnnots) { annotTpe =>
        if (realSymbol.annot(annotTpe).nonEmpty) Ok(())
        else Fail(s"it only accepts ${realSymbol.real.shortDescription}s annotated as $annotTpe")
      }.map(_ => ())
  }

  case class FallbackTag(annotTree: Tree) {
    def asList: List[Tree] = List(annotTree).filter(_ != EmptyTree)
    def orElse(other: FallbackTag): FallbackTag = FallbackTag(annotTree orElse other.annotTree)
  }
  object FallbackTag {
    final val Empty = FallbackTag(EmptyTree)
  }

  trait TagMatchingSymbol extends MacroSymbol with FilteringSymbol {
    def baseTagTpe: Type
    def fallbackTag: FallbackTag

    def annot(tpe: Type): Option[Annot] =
      findAnnotation(symbol, tpe)

    def tagAnnot(tpe: Type): Option[Annot] =
      annot(tpe)

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
            "cannot use @tagged, no tag annotation type specified"
          else s"tag annotation type $requiredTagType specified in @tagged annotation " +
            s"must be a subtype of specified base tag $baseTagTpe"
        reportProblem(msg)
      }
      val whenUntagged = FallbackTag(taggedAnnot.map(_.findArg[Tree](WhenUntaggedArg, EmptyTree)).getOrElse(EmptyTree))
      (requiredTagType, whenUntagged)
    }

    // returns fallback tag tree only IF it was necessary
    def matchTag(realSymbol: MacroSymbol): Res[FallbackTag] = {
      val tagAnnot = findAnnotation(realSymbol.symbol, baseTagTpe)
      val fallbackTagUsed = if (tagAnnot.isEmpty) whenUntaggedTag orElse fallbackTag else FallbackTag.Empty
      val realTagTpe = tagAnnot.map(_.tpe).getOrElse(NoType) orElse fallbackTagUsed.annotTree.tpe orElse baseTagTpe

      if (realTagTpe <:< requiredTag) Ok(fallbackTagUsed)
      else Fail(s"it only accepts ${realSymbol.shortDescription}s tagged with $requiredTag")
    }
  }

  trait ArityParam extends MacroParam with AritySymbol {
    def allowNamedMulti: Boolean
    def allowListedMulti: Boolean

    val arity: ParamArity =
      ParamArity.fromAnnotation(this, allowListedMulti, allowNamedMulti)

    lazy val optionLike: TermName = infer(tq"$OptionLikeCls[$actualType]")

    lazy val canBuildFrom: TermName = arity match {
      case _: ParamArity.Multi if allowNamedMulti && actualType <:< StringPFTpe =>
        infer(tq"$CanBuildFromCls[$NothingCls,($StringCls,${arity.collectedType}),$actualType]")
      case _: ParamArity.Multi =>
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

  trait MatchedSymbol {
    def real: MacroSymbol
    def annot(tpe: Type): Option[Annot]
    def allAnnots(tpe: Type): List[Annot]
    def rawName: String
    def indexInRaw: Int
  }

  trait SelfMatchedSymbol extends MacroSymbol with MatchedSymbol {
    def real: MacroSymbol = this
    def annot(tpe: Type): Option[Annot] = findAnnotation(symbol, tpe)
    def allAnnots(tpe: Type): List[Annot] = allAnnotations(symbol, tpe)
    def indexInRaw: Int = 0
    def rawName: String = nameStr
  }

  def collectParamMappings[Real <: MacroParam, Raw <: MacroParam, M](
    reals: List[Real], raws: List[Raw], rawShortDesc: String)(
    createMapping: (Raw, ParamsParser[Real]) => Res[M]): Res[List[M]] = {

    val parser = new ParamsParser(reals)
    Res.traverse(raws)(createMapping(_, parser)).flatMap { result =>
      if (parser.remaining.isEmpty) Ok(result)
      else {
        val unmatched = parser.remaining.iterator.map(_.nameStr).mkString(",")
        Fail(s"no $rawShortDesc(s) were found that would match real parameter(s) $unmatched")
      }
    }
  }

  class ParamsParser[Real <: MacroParam](reals: Seq[Real]) {

    import scala.collection.JavaConverters._

    private val realParams = new java.util.LinkedList[Real]
    realParams.addAll(reals.asJava)

    def remaining: Seq[Real] = realParams.asScala

    def extractSingle[M](consume: Boolean, matcher: Real => Option[Res[M]], unmatchedError: String): Res[M] = {
      val it = realParams.listIterator()
      def loop(): Res[M] =
        if (it.hasNext) {
          val real = it.next()
          matcher(real) match {
            case Some(res) =>
              if (consume) {
                it.remove()
              }
              res
            case None =>
              loop()
          }
        } else Fail(unmatchedError)
      loop()
    }

    def extractOptional[M](consume: Boolean, matcher: Real => Option[Res[M]]): Option[M] = {
      val it = realParams.listIterator()
      def loop(): Option[M] =
        if (it.hasNext) {
          val real = it.next()
          matcher(real) match {
            case Some(Ok(res)) =>
              if (consume) {
                it.remove()
              }
              Some(res)
            case Some(Fail(_)) => None
            case None => loop()
          }
        } else None
      loop()
    }

    def extractMulti[M](consume: Boolean, matcher: (Real, Int) => Option[Res[M]]): Res[List[M]] = {
      val it = realParams.listIterator()
      def loop(result: ListBuffer[M]): Res[List[M]] =
        if (it.hasNext) {
          val real = it.next()
          matcher(real, result.size) match {
            case Some(res) =>
              if (consume) {
                it.remove()
              }
              res match {
                case Ok(value) =>
                  result += value
                  loop(result)
                case fail: Fail =>
                  fail
              }
            case None => loop(result)
          }
        } else Ok(result.result())
      loop(new ListBuffer[M])
    }
  }
}
