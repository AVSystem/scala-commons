package com.avsystem.commons
package macros

import scala.reflect.macros.blackbox

/**
  * Author: ghik
  * Created: 26/11/15.
  */
trait MacroCommons {
  val c: blackbox.Context

  import c.universe._

  val CommonsPackage = q"_root_.com.avsystem.commons"
  val OptionCls = tq"_root_.scala.Option"

  implicit class treeOps[T <: Tree](t: T) {
    def debug: T = {
      c.echo(c.enclosingPosition, show(t))
      t
    }
  }

  def getType(typeTree: Tree): Type =
    c.typecheck(typeTree, c.TYPEmode).tpe

  def pathTo(sym: Symbol): Tree =
    if (sym == rootMirror.RootClass) Ident(termNames.ROOTPKG)
    else Select(pathTo(sym.owner), if (sym.isModuleClass) sym.name.toTermName else sym.name)

  def isTypeTree(tree: Tree) = tree match {
    case Ident(TypeName(_)) | Select(_, TypeName(_)) => true
    case _ => tree.isType
  }

  def select(pre: Tree, name: Name): Tree = pre match {
    case SingletonTypeTree(ref) if name.isTermName => select(ref, name)
    case t if isTypeTree(t) && name.isTypeName => SelectFromTypeTree(t, name.toTypeName)
    case t if name.isTermName => SingletonTypeTree(Select(t, name))
    case t => Select(t, name)
  }

  object ExistentialSingleton {
    def unapply(sym: Symbol): Option[(TypeSymbol, TermName, Type)] =
      if (sym.isType && sym.name.decodedName.toString.endsWith(".type")) {
        val ts = sym.asType
        if (ts.isExistential) ts.typeSignature match {
          case TypeBounds(lo, RefinedType(bases, scope))
            if lo =:= typeOf[Nothing] && bases.size >= 2 && bases.exists(_ =:= typeOf[Singleton]) =>

            val strName = sym.name.decodedName.toString.stripSuffix(".type")
            val newBases = bases.filterNot(_ =:= typeOf[Singleton])
            val newSig = newBases match {
              case List(singleBase) if scope.isEmpty => singleBase
              case _ => internal.refinedType(newBases, scope)
            }
            Some((ts, TermName(strName).encodedName.toTermName, newSig))
          case _ => None
        } else None
      } else None
  }

  /**
    * Returns a [[Tree]] that should typecheck to the type passed as argument (without using [[TypeTree]]).
    */
  def treeForType(tpe: Type): Tree = tpe match {
    case TypeRef(NoPrefix, ExistentialSingleton(_, name, _), Nil) =>
      Ident(name)
    case TypeRef(NoPrefix, sym, Nil) =>
      Ident(sym.name)
    case TypeRef(pre, sym, Nil) =>
      select(treeForType(pre), sym.name)
    case TypeRef(pre, sym, args) =>
      AppliedTypeTree(treeForType(internal.typeRef(pre, sym, Nil)), args.map(treeForType))
    case ThisType(sym) if sym.isPackage =>
      pathTo(sym)
    case ThisType(sym) =>
      SingletonTypeTree(This(sym.name.toTypeName))
    case SingleType(NoPrefix, sym) =>
      SingletonTypeTree(Ident(sym.name))
    case SingleType(pre, sym) =>
      select(treeForType(pre), sym.name)
    case TypeBounds(lo, hi) =>
      TypeBoundsTree(treeForType(lo), treeForType(hi))
    case RefinedType(parents, scope) =>
      val defns = scope.iterator.filterNot(s => s.isMethod && s.asMethod.isSetter).map {
        case ts: TypeSymbol => typeSymbolToTypeDef(ts)
        case ms: MethodSymbol if ms.isGetter => getterSymbolToValDef(ms)
        case ms: MethodSymbol => methodSymbolToDefDef(ms)
      }.toList
      CompoundTypeTree(Template(parents.map(treeForType), noSelfType, defns))
    case ExistentialType(quantified, underlying) =>
      ExistentialTypeTree(treeForType(underlying), quantified.map {
        case ExistentialSingleton(sym, name, signature) => existentialSingletonToValDef(sym, name, signature)
        case sym => typeSymbolToTypeDef(sym)
      })
    case NullaryMethodType(resultType) =>
      treeForType(resultType)
    case AnnotatedType(annots, underlying) =>
      annots.foldLeft(treeForType(underlying))((tree, annot) => Annotated(annot.tree, tree))
    case _ =>
      throw new Exception("Cannot create a tree that would represent " + showRaw(tpe))
  }

  def methodSymbolToDefDef(sym: Symbol): DefDef = {
    val ms = sym.asMethod
    val mods = Modifiers(Flag.DEFERRED, typeNames.EMPTY, Nil)
    val sig = ms.typeSignature
    val typeParams = sig.typeParams.map(typeSymbolToTypeDef)
    val paramss = sig.paramLists.map(_.map(paramSymbolToValDef))
    DefDef(mods, ms.name, typeParams, paramss, treeForType(sig.finalResultType), EmptyTree)
  }

  def paramSymbolToValDef(sym: Symbol): ValDef = {
    val ts = sym.asTerm
    val implicitFlag = if (sym.isImplicit) Flag.IMPLICIT else NoFlags
    val mods = Modifiers(Flag.PARAM | implicitFlag, typeNames.EMPTY, ts.annotations.map(_.tree))
    ValDef(mods, ts.name, treeForType(sym.typeSignature), EmptyTree)
  }

  def getterSymbolToValDef(sym: Symbol): ValDef = {
    val ms = sym.asMethod
    val mutableFlag = if (ms.isVar) Flag.MUTABLE else NoFlags
    val mods = Modifiers(Flag.DEFERRED | mutableFlag, typeNames.EMPTY, ms.annotations.map(_.tree))
    ValDef(mods, ms.name, treeForType(sym.typeSignature), EmptyTree)
  }

  def existentialSingletonToValDef(sym: Symbol, name: TermName, tpe: Type): ValDef = {
    val mods = Modifiers(Flag.DEFERRED, typeNames.EMPTY, sym.annotations.map(_.tree))
    ValDef(mods, name, treeForType(tpe), EmptyTree)
  }

  def typeSymbolToTypeDef(sym: Symbol): TypeDef = {
    val ts = sym.asType

    val paramOrDeferredFlag =
      if (ts.isParameter) Flag.PARAM
      else if (ts.isAbstract) Flag.DEFERRED
      else NoFlags
    val syntheticFlag = if (ts.isSynthetic) Flag.SYNTHETIC else NoFlags
    val varianceFlag =
      if (ts.isCovariant) Flag.COVARIANT
      else if (ts.isContravariant) Flag.CONTRAVARIANT
      else NoFlags

    val flags = paramOrDeferredFlag | syntheticFlag | varianceFlag
    val mods = Modifiers(flags, typeNames.EMPTY, ts.annotations.map(_.tree))
    val (typeParams, signature) = sym.typeSignature match {
      case PolyType(polyParams, resultType) => (polyParams, resultType)
      case sig => (ts.typeParams, sig)
    }
    TypeDef(mods, ts.name, typeParams.map(typeSymbolToTypeDef), treeForType(signature))
  }

  def alternatives(sym: Symbol): List[Symbol] = sym match {
    case ts: TermSymbol => ts.alternatives
    case NoSymbol => Nil
    case _ => List(sym)
  }

  case class ApplyUnapply(companion: Symbol, params: List[Symbol])

  def applyUnapplyFor(tpe: Type): ApplyUnapply = {
    val ts = tpe.typeSymbol.asType
    val companion = ts.companion

    if (companion == NoSymbol) {
      c.abort(c.enclosingPosition, s"$ts has no companion object")
    }
    val companionTpe = companion.asModule.moduleClass.asType.toType

    val applyUnapplyPairs = for {
      apply <- alternatives(companionTpe.member(TermName("apply")))
      unapply <- alternatives(companionTpe.member(TermName("unapply")))
    } yield (apply, unapply)

    def setTypeArgs(sig: Type) = sig match {
      case PolyType(params, resultType) => resultType.substituteTypes(params, tpe.typeArgs)
      case _ => sig
    }

    def typeParamsMatch(apply: Symbol, unapply: Symbol) = {
      val expected = tpe.typeArgs.length
      apply.typeSignature.typeParams.length == expected && unapply.typeSignature.typeParams.length == expected
    }

    val applicableResults = applyUnapplyPairs.flatMap {
      case (apply, unapply) if typeParamsMatch(apply, unapply) =>
        val applySig = setTypeArgs(apply.typeSignature)
        val unapplySig = setTypeArgs(unapply.typeSignature)

        applySig.paramLists match {
          case List(params) if applySig.finalResultType =:= tpe =>
            val expectedUnapplyTpe = params match {
              case Nil => typeOf[Boolean]
              case args => getType(tq"$OptionCls[(..${args.map(_.typeSignature)})]")
            }
            unapplySig.paramLists match {
              case List(List(soleArg)) if soleArg.typeSignature =:= tpe &&
                unapplySig.finalResultType =:= expectedUnapplyTpe =>

                Some(ApplyUnapply(companion, params))
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }

    applicableResults match {
      case List(result) =>
        result
      case Nil => c.abort(c.enclosingPosition,
        s"No suitable pair of apply/unapply methods found in companion object for $tpe")
      case _ => c.abort(c.enclosingPosition,
        s"Multiple suitable pairs of apply/unapply methods found in companion object for $tpe")
    }
  }

  def typeOfTypeSymbol(sym: TypeSymbol) = sym.toType match {
    case t@TypeRef(pre, s, Nil) if t.takesTypeArgs =>
      internal.typeRef(pre, s, t.typeParams.map(ts => internal.typeRef(NoPrefix, ts, Nil)))
    case t => t
  }

  def knownDirectSubtypes(tpe: Type): Option[List[Type]] =
    Option(tpe.typeSymbol).filter(s => s.isClass && s.isAbstract).map(_.asClass).filter(_.isSealed).map { sym =>
      val subclasses = sym.knownDirectSubclasses.toList
      if (subclasses.isEmpty) {
        c.abort(c.enclosingPosition, s"No subclasses found for sealed $sym. This may be caused by SI-7046")
      }
      subclasses.flatMap { subSym =>
        val undetparams = subSym.asType.typeParams
        val undetSubTpe = typeOfTypeSymbol(subSym.asType)
        val undetBaseTpe = undetSubTpe.baseType(sym)

        determineTypeParams(undetBaseTpe, tpe, undetparams)
          .map(typeArgs => undetSubTpe.substituteTypes(undetparams, typeArgs))
      }
    }

  sealed trait Variance {
    def *(other: Variance): Variance = (this, other) match {
      case (Covariant, Covariant) | (Contravariant, Contravariant) => Covariant
      case (Covariant, Contravariant) | (Contravariant, Covariant) => Contravariant
      case _ => Invariant
    }
  }
  object Variance {
    def apply(sym: Symbol): Variance =
      if (sym.isType) {
        val ts = sym.asType
        if (ts.isCovariant) Covariant
        else if (ts.isContravariant) Contravariant
        else Invariant
      } else Invariant
  }
  case object Invariant extends Variance
  case object Covariant extends Variance
  case object Contravariant extends Variance

  /**
    * Matches two types with each other to determine what are the "values" of some `typeParams` that occur
    * in `undetTpe`. [[None]] is returned when the types don't match each other or some of the type parameters
    * cannot be determined.
    *
    * This implementation only supports simple types, i.e. it only descends into type parameters. Things like
    * e.g. type refinements are not understood.
    */
  def determineTypeParams(undetTpe: Type, detTpe: Type, typeParams: List[Symbol]): Option[List[Type]] = {

    def checkConflict(m1: Map[Symbol, Type], m2: Map[Symbol, Type]): Unit =
      (m1.keySet intersect m2.keySet).foreach { k =>
        val tpe1 = m1(k)
        val tpe2 = m2(k)
        if (!(tpe1 =:= tpe2)) {
          c.abort(c.enclosingPosition, s"Type param $k could not be determined because it matches both $tpe1 and $tpe2")
        }
      }

    def merge(m1: Option[Map[Symbol, Type]], m2: Option[Map[Symbol, Type]]): Option[Map[Symbol, Type]] =
      for {
        fromM1 <- m1
        fromM2 <- m2
      } yield {
        checkConflict(fromM1, fromM2)
        fromM1 ++ fromM2
      }

    def isDetermined(tpe: Type) =
      !tpe.exists(t => typeParams.contains(t.typeSymbol))

    def withoutAnnotations(tpe: Type): Type = tpe match {
      case AnnotatedType(_, underlying) => withoutAnnotations(underlying)
      case _ => tpe
    }

    def baseType(tpe: Type, otherTpe: Type): Type =
      if (tpe <:< otherTpe && (tpe <:< typeOf[Nothing] || tpe <:< typeOf[Null])) otherTpe
      else tpe.baseType(otherTpe.typeSymbol)

    def determineTypeParamsIn(undetTpe: Type, detTpe: Type, variance: Variance): Option[Map[Symbol, Type]] =
      if (isDetermined(undetTpe)) {
        val typesMatch = variance match {
          case Covariant => undetTpe <:< detTpe
          case Contravariant => detTpe <:< undetTpe
          case Invariant => undetTpe =:= detTpe
        }
        if (typesMatch) Some(Map.empty) else None
      } else {
        def canMatch(tr1: TypeRef, tr2: TypeRef) = {
          val argsLength = tr1.args.length
          val typeParams1 = tr1.typeConstructor.typeParams
          val typeParams2 = tr2.typeConstructor.typeParams
          List(tr2.args, typeParams1, typeParams2).forall(_.length == argsLength)
        }

        def matchArgs = {
          val (undetBase, detBase) = variance match {
            case Covariant => (baseType(undetTpe, detTpe), detTpe)
            case Contravariant => (undetTpe, baseType(detTpe, undetTpe))
            case Invariant => (baseType(undetTpe, detTpe), baseType(detTpe, undetTpe))
          }
          (withoutAnnotations(undetBase), withoutAnnotations(detBase)) match {
            case (undetRef@TypeRef(_, _, undetArgs), detRef@TypeRef(_, _, detArgs)) if canMatch(undetRef, detRef) =>
              val undetTypeParams = undetRef.typeConstructor.typeParams
              val detTypeParams = detRef.typeConstructor.typeParams
              val variances = (undetTypeParams.map(Variance(_)) zip detTypeParams.map(Variance(_))).map {
                case (v1, v2) if v1 == v2 => variance * v1
                case _ => Invariant
              }
              (undetArgs, detArgs, variances).zipped.map {
                case (undetArg, detArg, newVariance) =>
                  determineTypeParamsIn(undetArg, detArg, newVariance)
              }.reduceOption(merge).getOrElse(Some(Map.empty[Symbol, Type]))
            case (NoType, _) | (_, NoType) =>
              None
            case (undet, det) =>
              c.abort(c.enclosingPosition,
                s"Could not determine type parameters in $undet when matching it against $det")
          }
        }

        withoutAnnotations(undetTpe) match {
          case TypeRef(NoPrefix, sym, Nil) if typeParams.contains(sym) =>
            Some(Map(sym -> detTpe))
          case _ => matchArgs
        }
      }

    val result = determineTypeParamsIn(undetTpe, detTpe, Covariant)
    result.map { mapping =>
      val stillUndetermined = (typeParams.toSet -- mapping.keySet).map(_.name)
      if (stillUndetermined.nonEmpty) {
        c.abort(c.enclosingPosition,
          s"Could not determine type parameters ${stillUndetermined.mkString(", ")} in $undetTpe when matching against $detTpe"
        )
      }
      typeParams.map(mapping)
    }
  }
}
