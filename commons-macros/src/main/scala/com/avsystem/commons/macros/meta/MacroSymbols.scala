package com.avsystem.commons
package macros.meta

import com.avsystem.commons.macros.MacroCommons
import com.avsystem.commons.macros.misc.{Fail, Ok, Res}

import scala.collection.mutable.ListBuffer

private[commons] trait MacroSymbols extends MacroCommons {

  import c.universe._

  final def RpcPackage = q"$CommonsPkg.rpc"
  final def MetaPackage = q"$CommonsPkg.meta"
  final def RpcUtils = q"$RpcPackage.RpcUtils"
  final def OptionLikeCls = tq"$MetaPackage.OptionLike"
  final def CanBuildFromCls = tq"$CollectionPkg.generic.CanBuildFrom"
  final lazy val RpcArityAT: Type = staticType(tq"$MetaPackage.SymbolArity")
  final lazy val SingleArityAT: Type = staticType(tq"$MetaPackage.single")
  final lazy val OptionalArityAT: Type = staticType(tq"$MetaPackage.optional")
  final lazy val MultiArityAT: Type = staticType(tq"$MetaPackage.multi")
  final lazy val FailArityAT: Type = staticType(tq"$MetaPackage.fail")
  final lazy val CompositeAT: Type = staticType(tq"$MetaPackage.composite")
  final lazy val AuxiliaryAT: Type = staticType(tq"$MetaPackage.auxiliary")
  final lazy val AnnotatedAT: Type = staticType(tq"$MetaPackage.annotated[_]")
  final lazy val TaggedAT: Type = staticType(tq"$RpcPackage.tagged[_]")
  final lazy val UnmatchedAT: Type = staticType(tq"$RpcPackage.unmatched")
  final lazy val WhenUntaggedArg: Symbol = TaggedAT.member(TermName("whenUntagged"))
  final lazy val FailArityErrorArg: Symbol = FailArityAT.member(TermName("error"))
  final lazy val UnmatchedErrorArg: Symbol = UnmatchedAT.member(TermName("error"))

  def primaryConstructor(ownerType: Type, ownerParam: Option[MacroSymbol]): Symbol =
    primaryConstructorOf(ownerType, ownerParam.fold("")(p => s"${p.problemStr}: "))

  sealed trait Arity {
    def annotStr: String
  }
  object Arity {
    trait Single extends Arity {
      final def annotStr = "@single"
    }
    trait Optional extends Arity {
      final def annotStr = "@optional"
    }
    trait Multi extends Arity {
      final def annotStr = "@multi"
    }
    trait Fail extends Arity {
      def error: String
      final def annotStr = "@fail"
    }
  }

  sealed abstract class ParamArity(val verbatimByDefault: Boolean) extends Arity {
    def collectedType: Type
  }
  object ParamArity {
    def fromAnnotation(
      param: ArityParam, allowListedMulti: Boolean, allowNamedMulti: Boolean, allowFail: Boolean
    ): ParamArity = {
      val annot = param.annot(RpcArityAT)
      val at = annot.fold(SingleArityAT)(_.tpe)
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
      else if (allowFail && at <:< FailArityAT) {
        if (param.actualType =:= UnitTpe)
          Fail(annot.get.findArg[String](FailArityErrorArg))
        else
          param.reportProblem(s"@fail ${param.shortDescription} must be typed as Unit")
      }
      else param.reportProblem(s"forbidden RPC arity annotation: $at")
    }

    case class Single(collectedType: Type) extends ParamArity(true) with Arity.Single
    case class Optional(collectedType: Type) extends ParamArity(true) with Arity.Optional
    case class Multi(collectedType: Type, named: Boolean) extends ParamArity(false) with Arity.Multi
    case class Fail(error: String) extends ParamArity(true) with Arity.Fail {
      def collectedType: Type = NothingTpe
    }
  }

  sealed abstract class MethodArity(val verbatimByDefault: Boolean) extends Arity
  object MethodArity {
    def fromAnnotation(method: MacroMethod): MethodArity = {
      val at = method.annot(RpcArityAT).fold(SingleArityAT)(_.tpe)
      if (at <:< SingleArityAT) Single
      else if (at <:< OptionalArityAT) Optional
      else if (at <:< MultiArityAT) Multi
      else method.reportProblem(s"unrecognized RPC method arity annotation: $at")
    }

    case object Single extends MethodArity(true) with Arity.Single
    case object Optional extends MethodArity(true) with Arity.Optional
    case object Multi extends MethodArity(false) with Arity.Multi
  }

  abstract class MacroSymbol {
    def symbol: Symbol
    def annotationSource: Symbol = symbol
    def pos: Position = symbol.pos
    def seenFrom: Type
    def shortDescription: String
    def description: String
    def problemStr: String = s"problem with $description"

    def reportProblem(msg: String, detailPos: Position = NoPosition): Nothing =
      abortAt(s"$problemStr: $msg", if (detailPos != NoPosition) detailPos else pos)

    def annot(tpe: Type, fallback: List[Tree] = Nil): Option[Annot] =
      findAnnotation(annotationSource, tpe, seenFrom, withInherited = true, fallback)

    def annots(tpe: Type, fallback: List[Tree] = Nil): List[Annot] =
      allAnnotations(annotationSource, tpe, seenFrom, withInherited = true, fallback)

    def infer(tpt: Tree): TermName =
      infer(getType(tpt))

    def infer(tpe: Type, forSym: MacroSymbol = this, clue: String = ""): TermName =
      inferCachedImplicit(tpe, s"${forSym.problemStr}: $clue", forSym.pos)

    val name: TermName = symbol.name.toTermName
    val safeName: TermName = c.freshName(name)
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
    def seenFrom: Type = ownerType

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
        else Fail()
      case _ => Ok(())
    }
  }

  trait FilteringSymbol extends MacroSymbol {
    lazy val requiredAnnots: List[Type] =
      annots(AnnotatedAT).map(_.tpe.dealias.typeArgs.head)

    lazy val unmatchedError: String =
      annot(UnmatchedAT).map(_.findArg[String](UnmatchedErrorArg))
        .getOrElse(s"${shortDescription.capitalize} $nameStr did not match it")

    def matchFilters(realSymbol: MatchedSymbol): Res[Unit] =
      Res.traverse(requiredAnnots) { annotTpe =>
        if (realSymbol.annot(annotTpe).nonEmpty) Ok(())
        else Fail()
      }.map(_ => ())
  }

  case class FallbackTag(annotTree: Tree) {
    def isEmpty: Boolean = annotTree == EmptyTree
    def asList: List[Tree] = List(annotTree).filter(_ != EmptyTree)
    def orElse(other: FallbackTag): FallbackTag = FallbackTag(annotTree orElse other.annotTree)
  }
  object FallbackTag {
    final val Empty = FallbackTag(EmptyTree)
  }

  trait TagMatchingSymbol extends MacroSymbol with FilteringSymbol {
    def baseTagTpe: Type
    def fallbackTag: FallbackTag

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
      val tagAnnot = realSymbol.annot(baseTagTpe)
      val fallbackTagUsed = if (tagAnnot.isEmpty) whenUntaggedTag orElse fallbackTag else FallbackTag.Empty
      val realTagTpe = tagAnnot.map(_.tpe).getOrElse(NoType) orElse fallbackTagUsed.annotTree.tpe orElse baseTagTpe

      if (realTagTpe <:< requiredTag) Ok(fallbackTagUsed)
      else Fail()
    }
  }

  trait ArityParam extends MacroParam with AritySymbol {
    def allowNamedMulti: Boolean
    def allowListedMulti: Boolean
    def allowFail: Boolean

    val arity: ParamArity =
      ParamArity.fromAnnotation(this, allowListedMulti, allowNamedMulti, allowFail)

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
    def rawName: String
    def indexInRaw: Int
    def fallbackTagUsed: FallbackTag = FallbackTag.Empty

    def annot(tpe: Type): Option[Annot] =
      real.annot(tpe, fallbackTagUsed.asList)

    def annots(tpe: Type): List[Annot] =
      real.annots(tpe, fallbackTagUsed.asList)
  }

  trait SelfMatchedSymbol extends MacroSymbol with MatchedSymbol {
    def real: MacroSymbol = this
    def indexInRaw: Int = 0
    def rawName: String = nameStr
  }

  def collectParamMappings[Real <: MacroParam, Raw <: MacroParam, M](
    reals: List[Real],
    raws: List[Raw],
    rawShortDesc: String,
    allowIncomplete: Boolean
  )(
    createMapping: (Raw, ParamsParser[Real]) => Res[M]
  ): Res[List[M]] = {

    val parser = new ParamsParser(reals)
    Res.traverse(raws)(createMapping(_, parser)).flatMap { result =>
      if (allowIncomplete || parser.remaining.isEmpty) Ok(result)
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

    def findFirst[M](matcher: Real => Option[M]): Option[M] = {
      val it = realParams.listIterator()
      def loop(): Option[M] =
        if (it.hasNext) {
          val real = it.next()
          matcher(real) match {
            case Some(m) => Some(m)
            case None => loop()
          }
        } else None
      loop()
    }
  }
}
