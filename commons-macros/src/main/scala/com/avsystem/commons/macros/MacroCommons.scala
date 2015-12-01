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

        val (existentialParams, underlyingTpe) = tpe match {
          case ExistentialType(quantified, underlying) => (quantified, underlying)
          case _ => (Nil, tpe)
        }
        determineTypeParams(underlyingTpe, undetBaseTpe, undetparams, existentialParams).map { typeArgs =>
          internal.existentialAbstraction(undetparams ++ existentialParams,
            undetSubTpe.substituteTypes(undetparams, typeArgs))
        }
      }
    }

  /**
    * Matches two types with each other to determine what are the "values" of some `typeParams` that occur
    * in `undetTpe`. [[None]] is returned when the types don't match each other or some of the type parameters
    * cannot be determined.
    */
  def determineTypeParams(detTpe: Type, undetTpe: Type, typeParams: List[Symbol], existentials: List[Symbol]): Option[List[Type]] = {

    def determineListTypeParams(detTpes: List[Type], undetTpes: List[Type], existentials: List[Symbol]): Option[Map[Symbol, Type]] =
      detTpes.zipAll(undetTpes, NoType, NoType).foldLeft(Option(Map.empty[Symbol, Type])) {
        case (_, (NoType, _) | (_, NoType)) => None
        case (acc, (detArg, undetArg)) =>
          for {
            fromAcc <- acc
            fromArg <- determineTypeParamsIn(detArg, undetArg, existentials)
          } yield fromAcc ++ fromArg
      }

    def determineTypeParamsIn(detTpe: Type, undetTpe: Type, existentials: List[Symbol]): Option[Map[Symbol, Type]] = {
      lazy val existentialDetTpe = internal.existentialAbstraction(existentials, detTpe)
      lazy val existentialUndetTpe = internal.existentialAbstraction(typeParams, undetTpe)

      (detTpe.dealias, undetTpe.dealias) match {
        case (tpe, TypeRef(NoPrefix, sym, Nil)) if typeParams.contains(sym) =>
          Some(Map(sym -> tpe))
        case (TypeRef(detPre, detSym, detArgs), TypeRef(undetPre, undetSym, undetArgs)) if detSym == undetSym =>
          for {
            fromPre <- determineTypeParamsIn(detPre, undetPre, existentials)
            fromArgs <- determineListTypeParams(detArgs, undetArgs, existentials)
          } yield fromArgs ++ fromPre
        case (SingleType(detPre, detSym), SingleType(undetPre, undetSym)) if detSym == undetSym =>
          determineTypeParamsIn(detPre, undetPre, existentials)
        case (SuperType(detThistpe, detSupertpe), SuperType(undetThistpe, undetSupertpe)) =>
          for {
            fromThistpe <- determineTypeParamsIn(detThistpe, undetThistpe, existentials)
            fromSupertpe <- determineTypeParamsIn(detSupertpe, undetSupertpe, existentials)
          } yield fromThistpe ++ fromSupertpe
        case (TypeBounds(detLo, detHi), TypeBounds(undetLo, undetHi)) =>
          for {
            fromLo <- determineTypeParamsIn(detLo, undetLo, existentials)
            fromHi <- determineTypeParamsIn(detHi, undetHi, existentials)
          } yield fromLo ++ fromHi
        case (BoundedWildcardType(detBounds), BoundedWildcardType(undetBounds)) =>
          determineTypeParamsIn(detBounds, undetBounds, existentials)
        case (MethodType(detParams, detResultType), MethodType(undetParams, undetResultType)) =>
          for {
            fromParams <- determineListTypeParams(detParams.map(_.typeSignature), undetParams.map(_.typeSignature), existentials)
            fromResultType <- determineTypeParamsIn(detResultType, undetResultType, existentials)
          } yield fromParams ++ fromResultType
        case (NullaryMethodType(detResultType), NullaryMethodType(undetResultType)) =>
          determineTypeParamsIn(detResultType, undetResultType, existentials)
        case (PolyType(typeParamsForDet, detResultType), undet@PolyType(typeParamsForUndet, undetResultType)) =>
          for {
            fromTypeParams <- determineListTypeParams(typeParamsForDet.map(_.typeSignature),
              typeParamsForUndet.map(_.typeSignature), existentials)
            detResultTypeSubst = detResultType.substituteSymbols(typeParamsForDet, typeParamsForUndet)
            fromResultType <- determineTypeParamsIn(detResultTypeSubst, undetResultType, existentials)
          } yield fromTypeParams ++ fromResultType
        case (ExistentialType(quantifiedForDet, detUnderlying), ExistentialType(quantifiedForUndet, undetUnderlying)) =>
          for {
            fromQuantified <- determineListTypeParams(quantifiedForDet.map(_.typeSignature),
              quantifiedForUndet.map(_.typeSignature), existentials)
            detUnderlyingSubst = detUnderlying.substituteSymbols(quantifiedForDet, quantifiedForUndet)
            fromUnderlying <- determineTypeParamsIn(detUnderlyingSubst, undetUnderlying, quantifiedForUndet ++ existentials)
          } yield fromQuantified ++ fromUnderlying
        case (RefinedType(detParents, detScope), RefinedType(undetParents, undetScope)) =>
          for {
            fromParents <- determineListTypeParams(detParents, undetParents, existentials)
            fromScope <- {
              val detMembersByName = detScope.toList.groupBy(_.name)
              val undetMembersByName = undetScope.toList.groupBy(_.name)
              val zero = Option(Map.empty[Symbol, Type])
              (detMembersByName.keySet ++ undetMembersByName.keySet).foldLeft(zero) { (acc, name) =>
                for {
                  fromAcc <- acc
                  detMembers <- detMembersByName.get(name)
                  undetMembers <- undetMembersByName.get(name)
                  // for overloaded members - find first permutation where overloads match each other
                  fromMembers <- undetMembers.permutations.toStream
                    .flatMap { undetMembersPerm =>
                      determineListTypeParams(detMembers.map(_.typeSignature), undetMembersPerm.map(_.typeSignature), existentials)
                    }.headOption
                } yield fromAcc ++ fromMembers
              }
            }
          } yield fromParents ++ fromScope
        case (AnnotatedType(_, detUnderlying), AnnotatedType(_, undetUnderlying)) =>
          determineTypeParamsIn(detUnderlying, undetUnderlying, existentials)
        case _ if detTpe =:= undetTpe =>
          Some(Map.empty)
        case _ if !(existentialUndetTpe <:< existentialDetTpe) =>
          // the types can never match
          None
        case _ =>
          // the types can match, but we are unable to determine type params
          c.abort(c.enclosingPosition, s"Could not determine type params in $undetTpe assuming it matches $detTpe")
      }
    }

    determineTypeParamsIn(detTpe, undetTpe, existentials).flatMap { mapping =>
      typeParams.foldRight(Option(List.empty[Type])) { (sym, accOpt) =>
        for {
          tpe <- mapping.get(sym)
          acc <- accOpt
        } yield tpe :: acc
      }
    }
  }
}

class TestMacros(val c: blackbox.Context) extends MacroCommons {

  import c.universe._

  def testTreeForType(tpeRepr: c.Tree): c.Tree = {
    val Literal(Constant(repr)) = tpeRepr

    val Typed(_, tpt) = c.parse(s"(??? : $repr)")
    val tpe = c.typecheck(tpt, mode = c.TYPEmode).tpe
    val newTree = treeForType(tpe)
    val newTpe = c.typecheck(newTree, mode = c.TYPEmode).tpe

    if (!(tpe =:= newTpe)) {
      c.error(c.enclosingPosition, s"Types don't match: $tpe and $newTpe")
    }
    q"???"
  }

  def testKnownDirectSubtypes[T: c.WeakTypeTag, R: c.WeakTypeTag]: c.Tree = {
    val expectedResultTpe = knownDirectSubtypes(weakTypeOf[T])
      .map(types => c.typecheck(tq"(..$types)", c.TYPEmode).tpe)
      .getOrElse(typeOf[Nothing])

    if (!(expectedResultTpe =:= weakTypeOf[R])) {
      c.abort(c.enclosingPosition, s"Types don't match, expected $expectedResultTpe")
    }
    q"???"
  }
}
