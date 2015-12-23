package com.avsystem.commons
package macros

import scala.collection.mutable
import scala.concurrent.Future
import scala.reflect.macros.{TypecheckException, blackbox}

/**
  * Author: ghik
  * Created: 26/11/15.
  */
trait MacroCommons {
  val c: blackbox.Context

  import c.universe._

  val CommonsPackage = q"_root_.com.avsystem.commons"
  val OptionCls = tq"_root_.scala.Option"
  val CollectionPkg = q"_root_.scala.collection"
  val ListObj = q"$CollectionPkg.immutable.List"
  val ListCls = tq"$CollectionPkg.immutable.List"
  val FutureSym = typeOf[Future[_]].typeSymbol

  lazy val ownerChain = {
    val sym = c.typecheck(q"val ${c.freshName(TermName(""))} = null").symbol
    Iterator.iterate(sym)(_.owner).takeWhile(_ != NoSymbol).drop(1).toList
  }

  lazy val enclosingClasses = {
    val sym = c.typecheck(q"this").tpe.typeSymbol
    Iterator.iterate(sym)(_.owner).takeWhile(_ != NoSymbol).toList
  }

  implicit class treeOps[T <: Tree](t: T) {
    def debug: T = {
      c.echo(c.enclosingPosition, show(t))
      t
    }
  }

  def abort(msg: String) =
    c.abort(c.enclosingPosition, msg)

  def echo(msg: String) =
    c.echo(c.enclosingPosition, msg)

  def error(msg: String) =
    c.error(c.enclosingPosition, msg)

  def warning(msg: String) =
    c.warning(c.enclosingPosition, msg)

  case class TypeKey(tpe: Type) {
    override def equals(obj: Any) = obj match {
      case TypeKey(otherTpe) => tpe =:= otherTpe
      case _ => false
    }
    override lazy val hashCode = {
      val dealiased = tpe.map(_.dealias)
      val innerSymbols = new mutable.HashSet[Symbol]
      def collectInnerSymbols(tpe: Type): Unit = tpe match {
        case PolyType(ts, _) =>
          innerSymbols ++= ts
        case ExistentialType(ts, _) =>
          innerSymbols ++= ts
        case rt@RefinedType(_, scope) =>
          innerSymbols += rt.typeSymbol
          innerSymbols ++= scope
          innerSymbols ++= scope.flatMap(_.typeSignature.typeParams)
          innerSymbols ++= scope.flatMap(_.typeSignature.paramLists.flatten)
        case _ =>
      }
      dealiased.foreach(collectInnerSymbols)
      val hashCodeSymbols = new mutable.ListBuffer[Symbol]
      dealiased.foreach {
        case ThisType(sym) if !innerSymbols(sym) =>
          hashCodeSymbols += sym
        case SingleType(_, sym) if !innerSymbols(sym) =>
          hashCodeSymbols += sym
        case TypeRef(_, sym, _) if !innerSymbols(sym) =>
          hashCodeSymbols += sym
        case _ =>
      }
      hashCodeSymbols.result().hashCode
    }
  }

  def implicitValue(tpe: Type, depTpe: Type): Tree =
    try c.typecheck(q"implicitly[$depTpe]") match {
      case Apply(_, List(arg)) => arg
    } catch {
      case te: TypecheckException =>
        abort(s"Auto derivation failed for $tpe: ${te.msg}")
    }

  def getType(typeTree: Tree): Type =
    c.typecheck(typeTree, c.TYPEmode).tpe

  def pathTo(sym: Symbol): Tree =
    if (enclosingClasses.contains(sym)) This(sym)
    else if (sym.isModuleClass) Select(pathTo(sym.owner), if (sym.isModuleClass) sym.name.toTermName else sym.name)
    else This(sym)

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
      select(treeForType(pre), if (sym.isModuleClass) sym.name.toTermName else sym.name)
    case TypeRef(pre, sym, args) =>
      AppliedTypeTree(treeForType(internal.typeRef(pre, sym, Nil)), args.map(treeForType))
    case ThisType(sym) if sym.isModuleClass =>
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
    val typeParams = sig.typeParams.map(typeSymbolToTypeDef(_, forMethod = true))
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

  def typeSymbolToTypeDef(sym: Symbol, forMethod: Boolean = false): TypeDef = {
    val ts = sym.asType

    val paramOrDeferredFlag =
      if (ts.isParameter) Flag.PARAM
      else if (ts.isAbstract) Flag.DEFERRED
      else NoFlags
    val syntheticFlag = if (ts.isSynthetic) Flag.SYNTHETIC else NoFlags
    val varianceFlag =
      if (forMethod) NoFlags
      else if (ts.isCovariant) Flag.COVARIANT
      else if (ts.isContravariant) Flag.CONTRAVARIANT
      else NoFlags

    val flags = paramOrDeferredFlag | syntheticFlag | varianceFlag
    val mods = Modifiers(flags, typeNames.EMPTY, ts.annotations.map(_.tree))
    val (typeParams, signature) = sym.typeSignature match {
      case PolyType(polyParams, resultType) => (polyParams, resultType)
      case sig => (ts.typeParams, sig)
    }
    TypeDef(mods, ts.name, typeParams.map(typeSymbolToTypeDef(_)), treeForType(signature))
  }

  def alternatives(sym: Symbol): List[Symbol] = sym match {
    case ts: TermSymbol => ts.alternatives
    case NoSymbol => Nil
    case _ => List(sym)
  }

  def unwrapNullaryMt(tpe: Type): Type = tpe match {
    case NullaryMethodType(underlying) => unwrapNullaryMt(underlying)
    case _ => tpe
  }

  case class ApplyUnapply(companion: Symbol, params: List[(Symbol, Tree)])

  def applyUnapplyFor(tpe: Type): Option[ApplyUnapply] = {
    val ts = tpe.typeSymbol.asType
    val companion = ts.companion

    if (companion != NoSymbol) {
      val companionTpe = getType(tq"$companion.type")

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
            case params :: implicits if implicits.flatten.forall(_.isImplicit) && applySig.finalResultType =:= tpe =>
              val expectedUnapplyTpe = params match {
                case Nil => typeOf[Boolean]
                case args => getType(tq"$OptionCls[(..${args.map(_.typeSignature)})]")
              }
              def defaultValueFor(param: Symbol, idx: Int): Tree =
                if (param.asTerm.isParamWithDefault)
                  q"$companion.${TermName("apply$default$" + (idx + 1))}[..${tpe.typeArgs}]"
                else EmptyTree

              unapplySig.paramLists match {
                case List(List(soleArg)) if soleArg.typeSignature =:= tpe &&
                  unapplySig.finalResultType =:= expectedUnapplyTpe =>

                  val paramsWithDefaults = params.zipWithIndex.map({ case (p, i) => (p, defaultValueFor(p, i)) })
                  Some(ApplyUnapply(companion, paramsWithDefaults))
                case _ => None
              }
            case _ => None
          }
        case _ => None
      }

      applicableResults match {
        case List(result) =>
          Some(result)
        case _ =>
          None
      }
    } else None
  }

  def singleValueFor(tpe: Type): Option[Tree] = tpe match {
    case ThisType(sym) if enclosingClasses.contains(sym) =>
      Some(This(sym))
    case ThisType(sym) if sym.isModuleClass =>
      singleValueFor(internal.thisType(sym.owner)).map(pre => Select(pre, tpe.termSymbol))
    case ThisType(sym) =>
      Some(This(sym))
    case SingleType(pre, sym) =>
      singleValueFor(pre).map(prefix => Select(prefix, sym))
    case ConstantType(value) =>
      Some(Literal(value))
    case TypeRef(pre, sym, Nil) if sym.isModuleClass =>
      singleValueFor(pre).map(prefix => Select(prefix, sym.asClass.module))
    case _ =>
      None
  }

  def typeOfTypeSymbol(sym: TypeSymbol) = sym.toType match {
    case t@TypeRef(pre, s, Nil) if t.takesTypeArgs =>
      internal.typeRef(pre, s, t.typeParams.map(ts => internal.typeRef(NoPrefix, ts, Nil)))
    case t => t
  }

  def isSealedHierarchyRoot(sym: Symbol) =
    sym.isClass && sym.isAbstract && sym.asClass.isSealed

  def knownNonAbstractSubclasses(sym: Symbol): Set[Symbol] =
    sym.asClass.knownDirectSubclasses.flatMap { s =>
      if (isSealedHierarchyRoot(s)) knownNonAbstractSubclasses(s) else Set(s)
    }

  def allCurrentlyKnownSubclasses(sym: Symbol): Set[Symbol] =
    if (sym.isClass) {
      val directSubclasses = sym.asClass.knownDirectSubclasses
      directSubclasses.flatMap(allCurrentlyKnownSubclasses) + sym
    } else Set.empty

  def withKnownSubclassesCheck(tree: Tree, tpe: Type): Tree =
    q"$tree: @$CommonsPackage.annotation.checkKnownSubtypes[$tpe](${allCurrentlyKnownSubclasses(tpe.typeSymbol).size})"

  def knownSubtypes(tpe: Type): Option[List[Type]] =
    Option(tpe.typeSymbol).filter(isSealedHierarchyRoot).map { sym =>
      knownNonAbstractSubclasses(sym).toList.flatMap { subSym =>
        val undetparams = subSym.asType.typeParams
        val undetSubTpe = typeOfTypeSymbol(subSym.asType)

        determineTypeParams(undetSubTpe, tpe, undetparams)
          .map(typeArgs => undetSubTpe.substituteTypes(undetparams, typeArgs))
      }
    }

  def determineTypeParams(undetTpe: Type, detTpe: Type, typeParams: List[Symbol]): Option[List[Type]] = {
    val methodName = c.freshName(TermName("m"))
    val typeDefs = typeParams.map(typeSymbolToTypeDef(_, forMethod = true))

    val tree = c.typecheck(
      q"""
        def $methodName[..$typeDefs](f: ${treeForType(undetTpe)} => Unit): Unit = ()
        $methodName((_: ${treeForType(detTpe)}) => ())
      """, silent = true
    )

    tree match {
      case Block(_, Apply(TypeApply(_, args), _)) => Some(args.map(_.tpe))
      case Block(_, Apply(_, _)) => Some(Nil)
      case EmptyTree => None
    }
  }
}
