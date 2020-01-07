package com.avsystem.commons
package macros

import com.avsystem.commons.macros.misc.{Ok, Res}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.macros.{TypecheckException, blackbox}
import scala.util.control.NoStackTrace
import scala.util.matching.Regex

abstract class AbstractMacroCommons(val c: blackbox.Context) extends MacroCommons

trait MacroCommons { bundle =>
  val c: blackbox.Context

  import c.universe._

  type ClassTag[T] = scala.reflect.ClassTag[T]
  final def classTag[T: ClassTag]: ClassTag[T] = scala.reflect.classTag[T]

  final def ScalaPkg: Tree = q"_root_.scala"
  final def JavaLangPkg: Tree = q"_root_.java.lang"
  final def StringCls: Tree = tq"$JavaLangPkg.String"
  final def IntCls: Tree = tq"$ScalaPkg.Int"
  final def ClassCls: Tree = tq"$JavaLangPkg.Class"
  final def NothingCls: Tree = tq"$ScalaPkg.Nothing"
  final def CommonsPkg: Tree = q"_root_.com.avsystem.commons"
  final def MiscPkg: Tree = q"$CommonsPkg.misc"
  final def PredefObj: Tree = q"$ScalaPkg.Predef"
  final def UnitCls: Tree = tq"$ScalaPkg.Unit"
  final def OptionCls: Tree = tq"$ScalaPkg.Option"
  final def OptionObj: Tree = q"$ScalaPkg.Option"
  final def SomeObj: Tree = q"$ScalaPkg.Some"
  final def NoneObj: Tree = q"$ScalaPkg.None"
  final def CollectionPkg: Tree = q"$ScalaPkg.collection"
  final def ImmutablePkg: Tree = q"$CollectionPkg.immutable"
  final def MutablePkg: Tree = q"$CollectionPkg.mutable"
  final def ListObj: Tree = q"$ImmutablePkg.List"
  final def ListCls: Tree = tq"$ImmutablePkg.List"
  final def SetObj: Tree = q"$ImmutablePkg.Set"
  final def SetCls: Tree = tq"$ImmutablePkg.Set"
  final def NilObj: Tree = q"$ImmutablePkg.Nil"
  final def MapObj: Tree = q"$ImmutablePkg.Map"
  final def MapCls: Tree = tq"$ImmutablePkg.Map"
  final def TryCls: Tree = tq"$ScalaPkg.util.Try"
  final def TryObj: Tree = q"$ScalaPkg.util.Try"
  final def ImplicitsObj: Tree = q"$MiscPkg.Implicits"
  final def ImplicitNotFoundCls: Tree = tq"$MiscPkg.ImplicitNotFound"
  final lazy val MapSym = typeOf[scala.collection.immutable.Map[_, _]].typeSymbol
  final lazy val FutureSym = typeOf[scala.concurrent.Future[_]].typeSymbol
  final lazy val OptionClass = definitions.OptionClass
  final lazy val AnnotationAggregateType = staticType(tq"$CommonsPkg.annotation.AnnotationAggregate")
  final lazy val DefaultsToNameAT = staticType(tq"$CommonsPkg.annotation.defaultsToName")
  final lazy val InferAT: Type = staticType(tq"$CommonsPkg.meta.infer")
  final lazy val NotInheritedFromSealedTypes = staticType(tq"$CommonsPkg.annotation.NotInheritedFromSealedTypes")
  final lazy val SeqCompanionSym = typeOf[scala.collection.Seq.type].termSymbol
  final lazy val PositionedAT = staticType(tq"$CommonsPkg.annotation.positioned")
  final lazy val ImplicitNotFoundAT = staticType(tq"$ScalaPkg.annotation.implicitNotFound")
  final lazy val ImplicitNotFoundSym = staticType(tq"$MiscPkg.ImplicitNotFound[_]").typeSymbol

  final lazy val UnitTpe: Type = definitions.UnitTpe
  final lazy val NothingTpe: Type = definitions.NothingTpe
  final lazy val StringPFTpe: Type = typeOf[PartialFunction[String, Any]]
  final lazy val BIterableTpe: Type = typeOf[Iterable[Any]]
  final lazy val BIndexedSeqTpe: Type = typeOf[IndexedSeq[Any]]
  final lazy val ProductTpe: Type = typeOf[Product]
  final lazy val AnnotationTpe: Type = typeOf[scala.annotation.Annotation]
  final lazy val EmptyTypeBounds: Type = internal.typeBounds(definitions.NothingTpe, definitions.AnyTpe)

  final def PartialFunctionClass: Symbol = StringPFTpe.typeSymbol
  final def BIterableClass: Symbol = BIterableTpe.typeSymbol
  final def BIndexedSeqClass: Symbol = BIndexedSeqTpe.typeSymbol

  implicit class debugOps[T](v: T) {
    def debug(prefix: String): T = {
      echo(prefix + ": " + show(v))
      v
    }
  }

  final lazy val isScalaJs =
    c.compilerSettings.exists(o => o.startsWith("-Xplugin:") && o.contains("scalajs-compiler"))

  final lazy val ownerChain = {
    val sym = typecheck(q"val ${c.freshName(TermName(""))} = null").symbol
    Iterator.iterate(sym)(_.owner).takeWhile(_ != NoSymbol).drop(1).toList
  }

  final lazy val enclosingClasses = {
    val enclosingSym = typecheck(q"this", silent = true) match {
      case EmptyTree => Iterator.iterate(c.internal.enclosingOwner)(_.owner).find(_.isModuleClass).get
      case tree => tree.tpe.typeSymbol
    }
    Iterator.iterate(enclosingSym)(_.owner).takeWhile(_ != NoSymbol).toList
  }

  final val debugEnabled: Boolean = c.prefix.tree match {
    case Select(Apply(_, List(_)), TermName("debugMacro")) => true
    case _ => false
  }

  final val statsEnabled: Boolean =
    c.settings.contains("statsEnabled")

  def debug(msg: => String): Unit =
    if (debugEnabled) {
      error(msg)
    }

  def instrument(tree: => Tree): Tree = measure(c.macroApplication.symbol.fullName) {
    val result = tree
    debug(show(result))
    result
  }

  private[this] var measureStack = List.empty[String]

  def measure[T](what: String)(expr: => T): T =
    if (!statsEnabled || measureStack.contains(what)) expr else {
      measureStack ::= what
      val start = System.nanoTime()
      try expr finally {
        echo(s"$what ${(System.nanoTime() - start) / 1000}")
        measureStack = measureStack.tail
      }
    }

  def inferImplicitValue(
    pt: Type,
    silent: Boolean = true,
    withMacrosDisabled: Boolean = false,
    expandMacros: Boolean = false,
    pos: Position = c.enclosingPosition
  ): Tree = {
    debug(s"macro implicit search for $pt")
    measure("implicit") {
      val basic = c.inferImplicitValue(pt, silent, withMacrosDisabled, pos)
      if (expandMacros && basic.exists(t => t.symbol != null && t.symbol.isMacro))
        c.typecheck(basic, silent = true, withMacrosDisabled = withMacrosDisabled)
      else basic
    }
  }

  def typecheck(
    tree: Tree,
    mode: c.TypecheckMode = c.TERMmode,
    pt: Type = WildcardType,
    silent: Boolean = false,
    withImplicitViewsDisabled: Boolean = false,
    withMacrosDisabled: Boolean = false
  ): Tree = measure("typecheck")(c.typecheck(tree, mode, pt, silent, withImplicitViewsDisabled, withMacrosDisabled))

  def containsInaccessibleThises(tree: Tree): Boolean = tree.exists {
    case t@This(_) if !t.symbol.isPackageClass && !enclosingClasses.contains(t.symbol) => true
    case _ => false
  }

  def indent(str: String, indent: String): String =
    str.replaceAllLiterally("\n", s"\n$indent")

  private def annotations(s: Symbol): List[Annotation] = {
    s.info // srsly scalac, load these goddamned annotations
    // for vals or vars, fetch annotations from underlying field
    if (s.isMethod && s.asMethod.isGetter)
      s.annotations ++ s.asMethod.accessed.annotations
    else s.annotations
  }

  def correctAnnotTree(tree: Tree, seenFrom: Type): Tree =
    treeAsSeenFrom(fixMethodTparamRefs(tree.duplicate), seenFrom)

  def treeAsSeenFrom(tree: Tree, seenFrom: Type): Tree = seenFrom match {
    case NoType => tree
    case TypeRef(_, sym, Nil) if sym.isStatic => tree
    case _ =>
      tree.foreach { t =>
        if (t.tpe != null) {
          internal.setType(t, t.tpe.asSeenFrom(seenFrom, seenFrom.typeSymbol))
        }
      }
      tree
  }

  // For some unknown reason references to method type params
  // in annotations are wrong and we have to replace them with correct method type params
  def fixMethodTparamRefs(tree: Tree): Tree = {
    tree.foreach { t =>
      if (t.tpe != null) {
        val newTpe = t.tpe.map {
          case it@TypeRef(NoPrefix, tp, targs) if tp.owner.isMethod =>
            tp.owner.asMethod.typeParams
              .find(mtp => mtp != tp && mtp.name == tp.name)
              .map(internal.typeRef(NoPrefix, _, targs))
              .getOrElse(it)
          case it => it
        }
        internal.setType(t, newTpe)
      }
      val sym = t.symbol
      if (sym != null && sym.isType && sym.isParameter && sym.owner.isMethod) {
        sym.owner.asMethod.typeParams
          .find(tp => tp != sym && tp.name == sym.name)
          .foreach(internal.setSymbol(t, _))
      }
    }
    tree
  }

  final class Annot(
    annotTree: Tree,
    val subject: Symbol,
    val directSource: Symbol,
    val aggregate: Option[Annot],
    paramMaterializer: Symbol => Option[Res[Tree]]
  ) {
    def aggregationChain: List[Annot] =
      aggregate.fold(List.empty[Annot])(a => a :: a.aggregationChain)

    def aggregationRootSource: Symbol = aggregate.fold(directSource)(_.aggregationRootSource)

    def errorPos: Option[Position] =
      List(tree.pos, directSource.pos, aggregationRootSource.pos, subject.pos).find(_ != NoPosition)

    lazy val constructorSig: Type = annotTree match {
      case Apply(Select(New(tpt), termNames.CONSTRUCTOR), _) =>
        val clsTpe = tpt.tpe
        primaryConstructorOf(clsTpe).typeSignatureIn(clsTpe)
    }

    lazy val treeRes: Res[Tree] = annotTree match {
      case Apply(constr, args) =>
        val newArgs = (args zip constructorSig.paramLists.head) map {
          case (arg, param) if param.asTerm.isParamWithDefault && arg.symbol != null &&
            arg.symbol.isSynthetic && arg.symbol.name.decodedName.toString.contains("$default$") =>
            if (findAnnotation(param, DefaultsToNameAT).nonEmpty)
              Ok(q"${subject.name.decodedName.toString}")
            else
              paramMaterializer(param).getOrElse(Ok(arg))
          case (arg, _) =>
            Ok(arg)
        }

        Res.sequence(newArgs).map(treeCopy.Apply(annotTree, constr, _))
      case _ =>
        Ok(annotTree)
    }

    def tree: Tree = treeRes.getOrElse { errOpt =>
      c.abort(errorPos.getOrElse(c.enclosingPosition), errOpt.getOrElse("unknown error"))
    }

    def tpe: Type =
      annotTree.tpe

    def symbol: ClassSymbol =
      tpe.typeSymbol.asClass

    lazy val argsByName: Map[Name, Tree] = {
      val Apply(_, args) = tree
      val paramNames = primaryConstructorOf(tpe).typeSignature.paramLists.head.map(_.name)
      (paramNames zip args).toMap
    }

    def findArg[T: ClassTag](valSym: Symbol): T =
      findArg[T](valSym, abort(s"(bug) no default value for ${tree.tpe} parameter ${valSym.name} provided by macro"))

    def findArg[T: ClassTag](valSym: Symbol, whenDefault: => T): T = tree match {
      case Apply(Select(New(tpt), termNames.CONSTRUCTOR), args) =>
        val clsTpe = tpt.tpe
        val params = primaryConstructorOf(clsTpe).typeSignature.paramLists.head
        ensure(params.size == args.size, s"Not a primary constructor call tree: $tree")
        val subSym = clsTpe.member(valSym.name)
        ensure(subSym.isTerm && subSym.asTerm.isParamAccessor && withSuperSymbols(subSym).contains(valSym),
          s"Annotation $clsTpe must override $valSym with a constructor parameter")
        (params zip args)
          .collectFirst {
            case (param, arg) if param.name == subSym.name => arg match {
              case Literal(Constant(value: T)) => value
              case t if param.asTerm.isParamWithDefault && t.symbol.isSynthetic &&
                t.symbol.name.decodedName.toString.contains("$default$") => whenDefault
              case t if classTag[T] == classTag[Tree] => t.asInstanceOf[T]
              case _ => abort(s"Expected literal ${classTag[T].runtimeClass.getSimpleName} " +
                s"as ${valSym.name} parameter of $clsTpe annotation")
            }
          }
          .getOrElse(abort(s"Could not find argument corresponding to constructor parameter ${subSym.name}"))
      case _ => abort(s"Not a primary constructor call tree: $tree")
    }

    private object argsInliner extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Select(th@This(_), name) if th.symbol == symbol && tree.symbol.asTerm.isParamAccessor =>
          argsByName.get(name).map(_.duplicate).getOrElse(tree)
        case _ => super.transform(tree)
      }
    }

    lazy val aggregated: List[Annot] = {
      if (tpe <:< AnnotationAggregateType) {
        val impliedMember = tpe.member(TypeName("Implied"))
        annotations(impliedMember).map { a =>
          val tree = argsInliner.transform(correctAnnotTree(a.tree, tpe))
          new Annot(tree, subject, impliedMember, Some(this), paramMaterializer)
        }
      } else Nil
    }

    lazy val withAllAggregated: List[Annot] =
      this :: aggregated.flatMap(_.withAllAggregated)

    override def toString: String = tree match {
      case Apply(Select(New(tpt), termNames.CONSTRUCTOR), args) =>
        s"@${showCode(tpt)}(${args.map(showCode(_)).mkString(",")})"
      case _ => showCode(tree)
    }
  }

  private def orConstructorParam(applyParam: Symbol): Symbol = {
    val owner = applyParam.owner
    if (owner.name == TermName("apply") && owner.isSynthetic)
      owner.owner.companion.asType.toType.member(termNames.CONSTRUCTOR).asMethod
        .paramLists.flatten.find(_.name == applyParam.name).getOrElse(applyParam)
    else applyParam
  }

  private def maybeWithSuperSymbols(s: Symbol, withSupers: Boolean): Iterator[Symbol] =
    if (withSupers) withSuperSymbols(s) else Iterator(s)

  def allAnnotations(
    s: Symbol,
    tpeFilter: Type,
    seenFrom: Type = NoType,
    withInherited: Boolean = true,
    fallback: List[Tree] = Nil,
    paramMaterializer: Symbol => Option[Res[Tree]] = _ => None
  ): List[Annot] = measure("annotations") {

    val initSym = orConstructorParam(s)
    def inherited(annot: Annotation, superSym: Symbol): Boolean =
      !(superSym != initSym && isSealedHierarchyRoot(superSym) && annot.tree.tpe <:< NotInheritedFromSealedTypes)

    val nonFallback = maybeWithSuperSymbols(initSym, withInherited)
      .flatMap(ss => annotations(ss).filter(inherited(_, ss))
        .map(a => new Annot(correctAnnotTree(a.tree, seenFrom), s, ss, None, paramMaterializer)))

    (nonFallback ++ fallback.iterator.map(t => new Annot(t, s, s, None, paramMaterializer)))
      .flatMap(_.withAllAggregated).filter(_.tpe <:< tpeFilter).toList
  }

  def findAnnotation(
    s: Symbol,
    tpe: Type,
    seenFrom: Type = NoType,
    withInherited: Boolean = true,
    fallback: List[Tree] = Nil,
    paramMaterializer: Symbol => Option[Res[Tree]] = _ => None
  ): Option[Annot] = measure("annotations") {
    val initSym = orConstructorParam(s)
    def find(annots: List[Annot], rejectDuplicates: Boolean): Option[Annot] = annots match {
      case head :: tail =>
        val fromHead = Some(head).filter(_.tpe <:< tpe).orElse(find(head.aggregated, rejectDuplicates))
        for {
          found <- fromHead
          ignored <- tail.filter(_.tpe <:< tpe) if rejectDuplicates
        } {
          val errorPos = ignored.errorPos.getOrElse(c.enclosingPosition)
          val aggInfo =
            if (ignored.aggregate.isEmpty) ""
            else ignored.aggregationChain.mkString(" (aggregated by ", " aggregated by", ")")
          c.error(errorPos, s"Annotation $ignored$aggInfo is ignored because it's overridden by $found")
        }
        fromHead orElse find(tail, rejectDuplicates)
      case Nil => None
    }

    def inherited(annot: Annotation, superSym: Symbol): Boolean =
      !(superSym != initSym && isSealedHierarchyRoot(superSym) && annot.tree.tpe <:< NotInheritedFromSealedTypes)

    maybeWithSuperSymbols(initSym, withInherited)
      .map(ss => find(
        annotations(ss).filter(inherited(_, ss))
          .map(a => new Annot(correctAnnotTree(a.tree, seenFrom), s, ss, None, paramMaterializer)),
        rejectDuplicates = true
      ))
      .collectFirst { case Some(annot) => annot }
      .orElse(find(fallback.map(t => new Annot(t, s, s, None, paramMaterializer)), rejectDuplicates = false))
  }

  def enclosingConstructorCompanion: Symbol =
    ownerChain.filter(_.isConstructor).map(_.owner.asClass.module).find(_ != NoSymbol).getOrElse(NoSymbol)

  // simplified representation of trees of implicits, used to remove duplicated implicits,
  // i.e. implicits that were found for different types but turned out to be identical
  private sealed trait ImplicitTrace
  private object ImplicitTrace {
    def apply(tree: Tree): Option[ImplicitTrace] =
      try Some(extract(tree)) catch {
        case TranslationFailed => None
      }

    private val TranslationFailed = new RuntimeException with NoStackTrace
    private def extract(tree: Tree): ImplicitTrace = tree match {
      case Literal(Constant(tpe: Type)) => Lit(TypeKey(tpe))
      case Literal(Constant(value)) => Lit(value)
      case Ident(_) if tree.symbol.isStatic => Static(tree.symbol)
      case Ident(name) => Id(name)
      case This(name) => Ths(name)
      case Select(pre, name) => Sel(extract(pre), name)
      case Apply(pre, args) => App(extract(pre), args.map(extract))
      case TypeApply(fun, _) => extract(fun)
      case Typed(expr, _) => extract(expr)
      case Annotated(_, arg) => extract(arg)
      case _ => throw TranslationFailed // this can probably only happen when implicit is a macro
    }

    case class Lit(value: Any) extends ImplicitTrace
    case class Id(name: Name) extends ImplicitTrace
    case class Ths(name: Name) extends ImplicitTrace
    case class Static(sym: Symbol) extends ImplicitTrace
    case class Sel(prefix: ImplicitTrace, name: Name) extends ImplicitTrace
    case class App(prefix: ImplicitTrace, args: List[ImplicitTrace]) extends ImplicitTrace
  }

  case class ErrorCtx(clue: String, pos: Position)

  sealed abstract class CachedImplicit {
    def actualType: Type
    def name: TermName
    def reference(allImplicitArgs: List[Tree]): Tree
  }
  case class RegisteredImplicit(name: TermName, actualType: Type) extends CachedImplicit {
    def reference(allImplicitArgs: List[Tree]): Tree = q"$name"
  }
  case class ImplicitDep(idx: Int, name: TermName, tpe: Type) {
    def mkImplicitParam: ValDef =
      ValDef(Modifiers(Flag.PARAM | Flag.IMPLICIT), name, tq"$tpe", EmptyTree)
  }
  case class InferredImplicit(
    name: TermName,
    tparams: List[Symbol],
    implicits: List[ImplicitDep],
    tpe: Type,
    body: Tree
  ) extends CachedImplicit {

    def actualType: Type = Option(body.tpe).getOrElse(NoType) orElse tpe

    def reference(allImplicitArgs: List[Tree]): Tree = {
      def collectArgs(idx: Int, deps: List[ImplicitDep], allArgs: List[Tree]): List[Tree] =
        (deps, allArgs) match {
          case (Nil, _) => Nil
          case (ImplicitDep(`idx`, _, _) :: depsRest, arg :: argsRest) =>
            arg :: collectArgs(idx + 1, depsRest, argsRest)
          case (_, _ :: argsRest) => collectArgs(idx + 1, deps, argsRest)
          case (_, Nil) => abort("too few arguments to satisfy implicit dependencies")
        }

      val implicitArgs = collectArgs(0, implicits, allImplicitArgs)
      val implicitArgss = if (implicitArgs.isEmpty) Nil else List(implicitArgs)
      q"$name[..${tparams.map(_.name)}](...$implicitArgss)"
    }

    def recreateDef(mods: Modifiers): Tree = {
      val typeDefs = tparams.map(typeSymbolToTypeDef(_, forMethod = true))
      val implicitDepDefs = List(implicits.map(_.mkImplicitParam)).filter(_.nonEmpty)
      stripTparamRefs(tparams) {
        q"$mods def $name[..$typeDefs](...$implicitDepDefs) = $ImplicitsObj.infer[$tpe]"
      }
    }

    def reusableBody: Boolean =
      tparams.isEmpty && implicits.isEmpty && body != EmptyTree && !body.exists(_.isDef)
  }

  private val implicitSearchCache = new mutable.HashMap[TypeKey, Option[CachedImplicit]]
  private val implicitsByTrace = new mutable.HashMap[ImplicitTrace, CachedImplicit]
  private val implicitsToDeclare = new mutable.ListBuffer[InferredImplicit]

  def tryInferCachedImplicit(
    tpe: Type,
    typeParams: List[Symbol] = Nil,
    availableImplicits: List[Type] = Nil,
    expandMacros: Boolean = false
  ): Option[CachedImplicit] = {

    def computeAsDef(): Option[CachedImplicit] = {
      val name = c.freshName(TermName("cachedImplicit"))
      val implicitDeps = availableImplicits.zipWithIndex.map {
        case (t, idx) => ImplicitDep(idx, c.freshName(TermName("dep")), t)
      }
      val ci = InferredImplicit(name, typeParams, implicitDeps, tpe, EmptyTree)
      val resolved = c.typecheck(ci.recreateDef(NoMods), silent = true)
      Option(resolved).collect { case dd: DefDef =>
        val actuallyUsedImplicitDeps =
          dd.vparamss.flatten.zip(implicitDeps).flatMap { case (vd, id) =>
            val used = dd.rhs.exists {
              case Ident(n) => n == vd.name
              case _ => false
            }
            if (used) Some(id) else None
          }

        val ciOptimized = ci.copy(implicits = actuallyUsedImplicitDeps, body = dd)
        ImplicitTrace(dd.rhs).flatMap { tr =>
          implicitsByTrace.get(tr).filter(_.actualType =:= ciOptimized.body.tpe)
        }.getOrElse {
          implicitsToDeclare += ciOptimized
          ciOptimized
        }
      }
    }

    def computeSingle(): Option[CachedImplicit] =
      Option(inferImplicitValue(tpe, expandMacros = expandMacros)).filter(_ != EmptyTree).map { found =>
        def newCachedImplicit(): CachedImplicit = {
          val name = c.freshName(TermName("cachedImplicit"))
          val ci = InferredImplicit(name, Nil, Nil, tpe, found)
          implicitsToDeclare += ci
          ci
        }
        ImplicitTrace(found).fold(newCachedImplicit()) { tr =>
          implicitsByTrace.get(tr).filter(_.actualType =:= found.tpe).getOrElse {
            val ci = newCachedImplicit()
            implicitsByTrace(tr) = ci
            ci
          }
        }
      }

    implicitSearchCache.getOrElseUpdate(TypeKey(tpe), {
      if (typeParams.isEmpty && availableImplicits.isEmpty) computeSingle()
      else computeAsDef()
    })
  }

  def inferCachedImplicit(
    tpe: Type,
    errorCtx: ErrorCtx,
    typeParams: List[Symbol] = Nil,
    availableImplicits: List[Type] = Nil,
    expandMacros: Boolean = false
  ): CachedImplicit =
    tryInferCachedImplicit(tpe, typeParams, availableImplicits, expandMacros).getOrElse {
      val rhsThatWillFail = q"$ImplicitsObj.infer[$tpe](${StringLiteral(errorCtx.clue, errorCtx.pos)})"
      val ci = InferredImplicit(c.freshName(TermName("")), Nil, Nil, tpe, rhsThatWillFail)
      implicitSearchCache(TypeKey(tpe)) = Some(ci)
      implicitsToDeclare += ci
      inferCachedImplicit(tpe, errorCtx, typeParams, availableImplicits, expandMacros)
    }

  def registerImplicit(tpe: Type, name: TermName): Unit = {
    implicitSearchCache(TypeKey(tpe)) = Some(RegisteredImplicit(name, tpe))
  }

  def cachedImplicitDeclarations: List[Tree] =
    cachedImplicitDeclarations(mkPrivateLazyValOrDef)

  def cachedImplicitDeclarations(mkDecl: InferredImplicit => Tree): List[Tree] =
    implicitsToDeclare.map(mkDecl).result()

  def mkPrivateLazyValOrDef(cachedImplicit: InferredImplicit): Tree =
    if (cachedImplicit.reusableBody)
      q"private lazy val ${cachedImplicit.name} = ${cachedImplicit.body}"
    else
      cachedImplicit.recreateDef(Modifiers(Flag.PRIVATE))

  private def standardImplicitNotFoundMsg(tpe: Type): String =
    symbolImplicitNotFoundMsg(tpe, tpe.typeSymbol, tpe.typeSymbol.typeSignature.typeParams, tpe.typeArgs)

  private def symbolImplicitNotFoundMsg(tpe: Type, sym: Symbol, tparams: List[Symbol], typeArgs: List[Type]): String =
    annotations(sym).find(_.tree.tpe <:< ImplicitNotFoundAT)
      .map(_.tree.children.tail.head).collect { case StringLiteral(error) => error }
      .map { error =>
        val tpNames = tparams.map(_.name.decodedName.toString)
        (tpNames zip typeArgs).foldLeft(error) {
          case (err, (tpName, tpArg)) => err.replaceAllLiterally(s"$${$tpName}", tpArg.toString)
        }
      }
      .getOrElse {
        if (sym != tpe.typeSymbol) standardImplicitNotFoundMsg(tpe)
        else s"no implicit value of type $tpe found"
      }

  private def replaceArgs(stack: List[Type], error: String, params: List[Symbol], args: List[Tree]): String =
    (params zip args)
      .flatMap { case (param, arg) =>
        arg.tpe.baseType(ImplicitNotFoundSym).typeArgs.headOption.map { delTpe =>
          param.name.decodedName.toString -> implicitNotFoundMsg(stack, delTpe, arg)
        }
      }
      .foldLeft(error) { case (err, (paramName, replacement)) =>
        err.replaceAllLiterally(s"#{$paramName}", replacement)
      }

  private def implicitNotFoundMsg(stack: List[Type], tpe: Type, tree: Tree): String =
    if (stack.exists(_ =:= tpe))
      standardImplicitNotFoundMsg(tpe)
    else tree match {
      case MaybeApply(MaybeTypeApply(fun, typeArgs), args) =>
        val sym = Option(fun.symbol).getOrElse(NoSymbol)
        val sig = sym.typeSignature
        val targs = typeArgs.map(_.tpe)
        val baseMsg = symbolImplicitNotFoundMsg(tpe, sym, sig.typeParams, targs)
        replaceArgs(tpe :: stack, baseMsg, sig.paramLists.headOption.getOrElse(Nil), args)
    }

  def implicitNotFoundMsg(tpe: Type): String =
    implicitNotFoundMsg(Nil, tpe, inferImplicitValue(getType(tq"$ImplicitNotFoundCls[$tpe]")))

  class treeOps[T <: Tree](t: T) {
    def debug: T = {
      c.echo(c.enclosingPosition, show(t))
      t
    }

    def asStatList: List[Tree] = t match {
      case Block(stats, expr) => stats :+ expr
      case _ => List(t)
    }
  }

  def paramIndex(param: Symbol): Int =
    param.owner.typeSignature.paramLists.flatten.indexOf(param) + 1

  def withSuperSymbols(s: Symbol): Iterator[Symbol] = s match {
    case cs: ClassSymbol => cs.baseClasses.iterator
    case ps: TermSymbol if ps.isParameter =>
      // if this is a `val` constructor parameter, include `val` itself and its super symbols
      accessorFor(ps).map(pa => Iterator(s) ++ withSuperSymbols(pa)).getOrElse {
        val oms = ps.owner.asMethod
        val paramListIdx = oms.paramLists.indexWhere(_.exists(_.name == ps.name))
        val paramIdx = oms.paramLists(paramListIdx).indexWhere(_.name == ps.name)
        withSuperSymbols(oms).map(_.asMethod.paramLists(paramListIdx)(paramIdx))
      }
    case ts: TermSymbol => (ts :: ts.overrides).iterator
    case ps: TypeSymbol if ps.isParameter && ps.owner.isMethod =>
      val oms = ps.owner.asMethod
      val paramIdx = oms.typeParams.indexWhere(_.name == ps.name)
      withSuperSymbols(oms).map(_.asMethod.typeParams(paramIdx))
    case ts: TypeSymbol => (ts :: ts.overrides).iterator
    case _ => Iterator(s)
  }

  def accessorFor(cparam: Symbol): Option[Symbol] =
    Option(cparam).filter(_.isParameter).map(_.owner.asMethod).filter(_.isPrimaryConstructor).map(_.owner.asClass)
      .flatMap(cs => alternatives(cs.toType.member(cparam.name)).find(_.asTerm.isParamAccessor))

  def primaryConstructorOf(tpe: Type, problemClue: => String = ""): Symbol =
    alternatives(tpe.member(termNames.CONSTRUCTOR)).find(_.asMethod.isPrimaryConstructor)
      .getOrElse(abort(s"${problemClue}no primary constructor found for $tpe"))

  def abort(msg: String): Nothing =
    c.abort(c.enclosingPosition, msg)

  def echo(msg: String): Unit =
    c.echo(c.enclosingPosition, msg)

  def error(msg: String): Unit =
    c.error(c.enclosingPosition, msg)

  def warning(msg: String): Unit =
    c.warning(c.enclosingPosition, msg)

  def ensure(condition: Boolean, msg: => String): Unit =
    if (!condition) {
      abort(msg)
    }

  def posInfo(pos: Position): String =
    s"${pos.source.file.name}:${pos.line}:${pos.column}"

  def abortAt(message: String, pos: Position): Nothing =
    if (pos != NoPosition && pos != c.enclosingPosition) {
      if (pos.source == c.enclosingPosition.source) {
        c.abort(pos, s"Macro at line ${c.enclosingPosition.line} failed: $message")
      } else {
        c.error(pos, s"Macro at ${posInfo(c.enclosingPosition)} failed: $message")
        abort(s"Macro expansion failed because of error at ${posInfo(pos)}")
      }
    } else {
      abort(message)
    }

  def errorAt(message: String, pos: Position): Unit = {
    if (pos != NoPosition && pos != c.enclosingPosition) {
      val posRepr =
        if (pos.source == c.enclosingPosition.source) s"line ${c.enclosingPosition.line}"
        else posInfo(c.enclosingPosition)
      c.error(pos, s"Macro at $posRepr failed: $message")
    } else {
      error(message)
    }
  }

  /**
    * Wrapper over Type that implements equals/hashCode consistent with type equivalence (=:=)
    */
  case class TypeKey(tpe: Type) {
    override def equals(obj: Any): Boolean = obj match {
      case TypeKey(otherTpe) => tpe =:= otherTpe
      case _ => false
    }
    override lazy val hashCode: Int = {
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

  def staticType(tpeTree: Tree): Type =
    getType(tpeTree)

  def getType(typeTree: Tree): Type =
    measure("typecheck.getType")(typecheck(typeTree, c.TYPEmode).tpe)

  def pathTo(sym: Symbol): Tree =
    if (enclosingClasses.contains(sym)) This(sym)
    else if (sym.isModuleClass) Select(pathTo(sym.owner), if (sym.isModuleClass) sym.name.toTermName else sym.name)
    else This(sym)

  def isTypeTree(tree: Tree): Boolean = tree match {
    case Ident(TypeName(_)) | Select(_, TypeName(_)) => true
    case _ => tree.isType
  }

  def select(pre: Tree, name: Name): Tree = pre match {
    case SingletonTypeTree(ref) if name.isTermName => select(ref, name)
    case t if isTypeTree(t) && name.isTypeName => SelectFromTypeTree(t, name.toTypeName)
    case t if name.isTermName => SingletonTypeTree(Select(t, name))
    case t => Select(t, name)
  }

  object StringLiteral {
    def unapply(tree: Tree): Option[String] = tree match {
      case Literal(Constant(str: String)) => Some(str)
      case _ => None
    }
    def apply(str: String, pos: Position = NoPosition): Tree =
      internal.setPos(Literal(Constant(str)), pos)
  }

  object BooleanLiteral {
    def unapply(tree: Tree): Option[Boolean] = tree match {
      case Literal(Constant(boolean: Boolean)) => Some(boolean)
      case _ => None
    }
  }

  object Lit {
    def unapply(tree: Tree): Option[Any] = tree match {
      case Literal(Constant(value)) => Some(value)
      case _ => None
    }
  }

  case class LitOrDefault[T: ClassTag](default: T) {
    def unapply(tree: Tree): Option[T] = tree match {
      case Literal(Constant(value: T)) => Some(value)
      case Select(_, TermName(n)) if n.startsWith("$lessinit$greater$default$") => Some(default)
      case _ => None
    }
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

  object AnonPartialFunction {
    def unapply(tree: Tree): Option[List[CaseDef]] = tree match {
      case Typed(Block(List(ClassDef(_, _, _, Template(_, _, List(_, DefDef(_, _, _, _, _, Match(_, cases)), _)))), _), _)
        if tree.tpe <:< typeOf[PartialFunction[Nothing, Any]] =>
        Some(cases)
      case _ => None
    }
  }

  object MaybeTyped {
    def unapply(t: Tree): Some[(Tree, Option[Tree])] = t match {
      case Typed(expr, tpt) => Some((expr, Some(tpt)))
      case _ => Some(t, None)
    }
  }

  object MaybeTypeApply {
    def unapply(tree: Tree): Some[(Tree, List[Tree])] = tree match {
      case TypeApply(fun, args) => Some((fun, args))
      case _ => Some((tree, Nil))
    }
  }

  object MaybeApply {
    def unapply(tree: Tree): Some[(Tree, List[Tree])] = tree match {
      case Apply(fun, args) => Some((fun, args))
      case _ => Some((tree, Nil))
    }
  }

  object MultiApply {
    def unapply(tree: Tree): Some[(Tree, List[List[Tree]])] = {
      def collect(tree: Tree, argLists: List[List[Tree]]): (Tree, List[List[Tree]]) = tree match {
        case Apply(fun, args) => collect(fun, args :: argLists)
        case fun => (fun, argLists)
      }
      Some(collect(tree, Nil))
    }
  }

  object MaybeExistentialType {
    def unapply(tpe: Type): Some[(List[Symbol], Type)] = tpe match {
      case ExistentialType(quantified, underlying) => Some((quantified, underlying))
      case _ => Some((Nil, tpe))
    }
  }

  object WrappedImplicitSearchResult {
    def unapply(tree: Tree): Option[Tree] = tree match {
      case Block(_, expr) => unapply(expr)
      case Apply(_, List(arg)) => Some(arg)
    }
  }

  private val DefaultValueMethodName: Regex = """(.*)\$default\$(\d+)$""".r

  object DefaultValueMethod {
    def unapply(s: Symbol): Option[Symbol] = s match {
      case ms: MethodSymbol if ms.isSynthetic => ms.name.encodedName.toString match {
        case DefaultValueMethodName(name, idx) =>
          val actualMethodName = TermName(name).decodedName
          val paramIndex = idx.toInt - 1
          val ownerMethod = actualMethodName match {
            case termNames.CONSTRUCTOR =>
              ms.owner.companion.asType.toType.member(termNames.CONSTRUCTOR)
            case _ =>
              ms.owner.asType.toType.member(actualMethodName)
          }
          Some(ownerMethod.asMethod.paramLists.flatten.apply(paramIndex))
        case _ => None
      }
      case _ => None
    }
  }

  def defaultValueMethod(param: Symbol): Symbol =
    if (param.isTerm && param.isParameter && param.asTerm.isParamWithDefault) {
      val owner = param.owner.asMethod
      val idx = owner.paramLists.flatten.indexOf(param) + 1
      val dvMethodOwner =
        if (owner.isConstructor) owner.owner.companion else owner.owner
      dvMethodOwner.asType.toType.member(TermName(s"${owner.name.encodedName.toString}$$default$$$idx"))
    } else NoSymbol

  /**
    * Returns a `Tree` that should typecheck to the type passed as argument (without using `TypeTree`).
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
      val loTree = if (lo =:= definitions.NothingTpe) EmptyTree else treeForType(lo)
      val hiTree = if (hi =:= definitions.AnyTpe) EmptyTree else treeForType(hi)
      TypeBoundsTree(loTree, hiTree)
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
    case PolyType(tparams, result) =>
      val tcname = c.freshName(TypeName("tc"))
      val mods = Modifiers(NoFlags, typeNames.EMPTY, Nil)
      val typedef = TypeDef(mods, tcname, tparams.map(typeSymbolToTypeDef(_)), treeForType(result))
      val parents = List(pathTo(definitions.AnyRefClass))
      SelectFromTypeTree(CompoundTypeTree(Template(parents, noSelfType, List(typedef))), tcname)
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
    val mods = Modifiers(Flag.PARAM | implicitFlag, typeNames.EMPTY, annotations(ts).map(_.tree))
    ValDef(mods, ts.name, treeForType(sym.typeSignature), EmptyTree)
  }

  def getterSymbolToValDef(sym: Symbol): ValDef = {
    val ms = sym.asMethod
    val mutableFlag = if (ms.isVar) Flag.MUTABLE else NoFlags
    val mods = Modifiers(Flag.DEFERRED | mutableFlag, typeNames.EMPTY, annotations(ms).map(_.tree))
    ValDef(mods, ms.name, treeForType(sym.typeSignature), EmptyTree)
  }

  def existentialSingletonToValDef(sym: Symbol, name: TermName, tpe: Type): ValDef = {
    val mods = Modifiers(Flag.DEFERRED, typeNames.EMPTY, annotations(sym).map(_.tree))
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
    val mods = Modifiers(flags, typeNames.EMPTY, annotations(ts).map(_.tree))
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

  def isFirstListVarargs(meth: Symbol): Boolean =
    meth.typeSignature.paramLists.headOption.flatMap(_.lastOption).exists(isRepeated)

  def actualParamType(param: Symbol): Type =
    actualParamType(param.typeSignature)

  def actualParamType(tpe: Type): Type = tpe match {
    case TypeRef(_, s, List(arg)) if s == definitions.RepeatedParamClass =>
      getType(tq"$ScalaPkg.Seq[$arg]")
    case TypeRef(_, s, List(arg)) if s == definitions.ByNameParamClass =>
      arg
    case _ => tpe
  }

  def isRepeated(param: Symbol): Boolean =
    param.typeSignature.typeSymbol == definitions.RepeatedParamClass

  def isParameterless(signature: Type): Boolean =
    signature.paramLists.flatten == Nil

  def hasMemberWithSig(tpe: Type, name: Name, suchThat: Type => Boolean): Boolean =
    alternatives(tpe.member(name)).exists(sym => suchThat(sym.typeSignatureIn(tpe)))

  object SingleParamList {
    def unapply(paramLists: List[List[Symbol]]): Option[List[Symbol]] = paramLists match {
      case head :: tail if tail.flatten.forall(_.isImplicit) => Some(head)
      case _ => None
    }
  }

  def matchingApplyUnapply(tpe: Type, applySig: Type, unapplySig: Type): Boolean = {
    applySig.finalResultType =:= tpe && (unapplySig.paramLists match {
      case SingleParamList(List(unapplyParam)) if unapplyParam.typeSignature =:= tpe =>
        applySig.paramLists match {
          case SingleParamList(args) => isCorrectUnapply(unapplySig.finalResultType, args)
          case _ => false
        }
      case _ => false
    })
  }

  def isCorrectUnapply(unapplyResultType: Type, applyParams: List[Symbol], elemAdjust: Type => Type = identity): Boolean =
    unapplyResultType match {
      // https://issues.scala-lang.org/browse/SI-6541 (fixed in Scala 2.11.5 but requires -Xsource:2.12)
      case ExistentialType(quantified, underlying) =>
        isCorrectUnapply(underlying, applyParams, internal.existentialAbstraction(quantified, _))
      case tpe =>
        def hasIsEmpty = hasMemberWithSig(tpe, TermName("isEmpty"),
          sig => isParameterless(sig) && sig.finalResultType =:= typeOf[Boolean])

        def hasProperGet(resultTypeCondition: Type => Boolean): Boolean =
          hasMemberWithSig(tpe, TermName("get"), sig => isParameterless(sig) && resultTypeCondition(sig.finalResultType))

        applyParams match {
          case Nil =>
            tpe =:= typeOf[Boolean]
          case List(singleParam) =>
            hasIsEmpty && hasProperGet(resType => elemAdjust(resType) =:= actualParamType(singleParam.typeSignature))
          case params =>
            hasIsEmpty && hasProperGet { resType =>
              val elemTypes = Iterator.range(1, 22).map { i =>
                alternatives(resType.member(TermName(s"_$i")))
                  .map(_.typeSignatureIn(resType))
                  .find(sig => sig.typeParams == Nil && sig.paramLists == Nil)
                  .map(sig => elemAdjust(sig.finalResultType))
                  .getOrElse(NoType)
              }.takeWhile(_ != NoType).toList

              def check(params: List[Symbol], elemTypes: List[Type]): Boolean = (params, elemTypes) match {
                case (Nil, Nil) => true
                case (p :: prest, et :: etrest) =>
                  actualParamType(p.typeSignature) =:= et && check(prest, etrest)
                case _ => false
              }

              check(params, elemTypes)
            }
        }
    }

  /**
    * @param apply   case class constructor or companion object's apply method
    * @param unapply companion object'a unapply method or `NoSymbol` for case class with more than 22 fields
    * @param params  parameters with trees evaluating to default values (or `EmptyTree`s)
    */
  case class ApplyUnapply(ownerTpe: Type, typedCompanion: Tree, apply: Symbol, unapply: Symbol, params: List[TermSymbol]) {
    def standardCaseClass: Boolean = apply.isConstructor

    def synthetic: Boolean = (apply.isConstructor || apply.isSynthetic) && unapply.isSynthetic

    def defaultValueFor(param: Symbol): Tree =
      defaultValueFor(param, params.indexOf(param))

    def defaultValueFor(param: Symbol, idx: Int): Tree =
      if (param.asTerm.isParamWithDefault) {
        val methodEncodedName = param.owner.name.encodedName.toString
        q"$typedCompanion.${TermName(s"$methodEncodedName$$default$$${idx + 1}")}[..${ownerTpe.typeArgs}]"
      }
      else EmptyTree

    def mkApply[T: Liftable](args: Seq[T]): Tree =
      if (standardCaseClass) q"new $ownerTpe(..$args)"
      else q"$typedCompanion.apply[..${ownerTpe.typeArgs}](..$args)"
  }

  def applyUnapplyFor(tpe: Type): Option[ApplyUnapply] =
    typedCompanionOf(tpe).flatMap(comp => applyUnapplyFor(tpe, comp))

  def applyUnapplyFor(tpe: Type, typedCompanion: Tree): Option[ApplyUnapply] = measure("applyUnapplyFor") {
    val dtpe = tpe.dealias
    val ts = dtpe.typeSymbol.asType
    val caseClass = ts.isClass && ts.asClass.isCaseClass

    def params(methodSig: Type): List[TermSymbol] =
      methodSig.paramLists.head.map(_.asTerm)

    // Seq is a weird corner case where technically an apply/unapplySeq pair exists but is recursive
    val applyUnapplyPairs =
      if (typedCompanion.symbol == SeqCompanionSym) Nil
      else for {
        apply <- alternatives(typedCompanion.tpe.member(TermName("apply")))
        unapplyName = if (isFirstListVarargs(apply)) "unapplySeq" else "unapply"
        unapply <- alternatives(typedCompanion.tpe.member(TermName(unapplyName)))
      } yield (apply, unapply)

    def setTypeArgs(sig: Type) = sig match {
      case PolyType(params, resultType) => resultType.substituteTypes(params, dtpe.typeArgs)
      case _ => sig
    }

    def typeParamsMatch(apply: Symbol, unapply: Symbol) = {
      val expected = dtpe.typeArgs.length
      apply.typeSignature.typeParams.length == expected && unapply.typeSignature.typeParams.length == expected
    }

    if (caseClass && applyUnapplyPairs.isEmpty) { // case classes with more than 22 fields
      val constructor = primaryConstructorOf(dtpe)
      Some(ApplyUnapply(dtpe, typedCompanion, constructor, NoSymbol, params(constructor.typeSignatureIn(dtpe))))
    } else {
      val applicableResults = applyUnapplyPairs.flatMap {
        case (apply, unapply) if caseClass && apply.isSynthetic && unapply.isSynthetic =>
          val constructor = primaryConstructorOf(dtpe)
          Some(ApplyUnapply(dtpe, typedCompanion, constructor, unapply, params(constructor.typeSignatureIn(dtpe))))
        case (apply, unapply) if typeParamsMatch(apply, unapply) =>
          val applySig =
            setTypeArgs(apply.typeSignatureIn(typedCompanion.tpe))
          val unapplySig =
            setTypeArgs(unapply.typeSignatureIn(typedCompanion.tpe))
          if (matchingApplyUnapply(dtpe, applySig, unapplySig))
            Some(ApplyUnapply(dtpe, typedCompanion, apply, unapply, params(applySig)))
          else None
        case _ => None
      }

      def choose(results: List[ApplyUnapply]): Option[ApplyUnapply] = results match {
        case Nil => None
        case List(result) => Some(result)
        case multiple if multiple.exists(_.synthetic) =>
          // prioritize non-synthetic apply/unapply pairs
          choose(multiple.filterNot(_.synthetic))
        case _ => None
      }

      choose(applicableResults)
    }
  }

  def singleValueFor(tpe: Type): Option[Tree] = measure("singleValueFor")(tpe match {
    case ThisType(sym) if enclosingClasses.contains(sym) =>
      Some(This(sym))
    case ThisType(sym) if sym.isModuleClass =>
      singleValueFor(internal.thisType(sym.owner)).map(pre => Select(pre, tpe.termSymbol))
    case ThisType(sym) =>
      Some(This(sym))
    case SingleType(NoPrefix, sym) =>
      Some(Ident(sym))
    case SingleType(pre, sym) =>
      singleValueFor(pre).map(prefix => Select(prefix, sym))
    case ConstantType(value) =>
      Some(Literal(value))
    case TypeRef(pre, sym, Nil) if sym.isModuleClass =>
      singleValueFor(pre).map(prefix => Select(prefix, sym.asClass.module))
    case _ =>
      None
  })

  def typedCompanionOf(tpe: Type): Option[Tree] = {
    val result = tpe match {
      case TypeRef(pre, sym, _) if sym.companion != NoSymbol =>
        singleValueFor(pre).map(Select(_, sym.companion)) orElse singleValueFor(tpe.companion)
      case _ => singleValueFor(tpe.companion)
    }
    result.map(typecheck(_))
  }

  def typeOfTypeSymbol(sym: TypeSymbol): Type = sym.toType match {
    case t@TypeRef(pre, s, Nil) if t.takesTypeArgs =>
      internal.typeRef(pre, s, t.typeParams.map(ts => internal.typeRef(NoPrefix, ts, Nil)))
    case t => t
  }

  def isSealedHierarchyRoot(sym: Symbol): Boolean = {
    sym.info // force loading of type information, sometimes it may be missing when loading from classfile
    sym.isClass && sym.isAbstract && sym.asClass.isSealed
  }

  def knownNonAbstractSubclasses(sym: Symbol): Set[Symbol] =
    sym.asClass.knownDirectSubclasses.flatMap { s =>
      if (isSealedHierarchyRoot(s)) knownNonAbstractSubclasses(s) else Set(s)
    }

  def allCurrentlyKnownSubclasses(sym: Symbol): Set[Symbol] =
    if (sym.isClass) {
      val directSubclasses = sym.asClass.knownDirectSubclasses
      directSubclasses.flatMap(allCurrentlyKnownSubclasses) + sym
    } else Set.empty

  private val ownersCache = new mutable.OpenHashMap[Symbol, List[Symbol]]
  def ownersOf(sym: Symbol): List[Symbol] =
    ownersCache.getOrElseUpdate(sym, Iterator.iterate(sym)(_.owner).takeWhile(_ != NoSymbol).toList.reverse)

  private val positionCache = new mutable.OpenHashMap[Symbol, Int]
  def positionPoint(sym: Symbol): Int =
    if (c.enclosingPosition.source == sym.pos.source) sym.pos.point
    else positionCache.getOrElseUpdate(sym,
      annotations(sym).find(_.tree.tpe <:< PositionedAT).map(_.tree).map {
        case Apply(_, List(MaybeTyped(Lit(point: Int), _))) => point
        case t => abort(s"expected literal int as argument of @positioned annotation on $sym, got $t")
      } getOrElse {
        abort(s"Could not determine source position of $sym - " +
          s"it resides in separate file than macro invocation and has no @positioned annotation")
      })

  def innerTypeSymbols(tpe: Type): Set[Symbol] = {
    val result = Set.newBuilder[Symbol]
    tpe.foreach {
      case PolyType(tparams, _) =>
        result ++= tparams
      case ExistentialType(quantified, _) =>
        result ++= quantified
      case RefinedType(_, scope) =>
        result ++= scope.iterator.filter(_.isType)
      case _ =>
    }
    result.result()
  }

  def outerTypeParamsIn(tpe: Type): List[Symbol] = {
    val innerTparams = innerTypeSymbols(tpe)
    val result = new ListBuffer[Symbol]
    tpe.foreach { t =>
      val ts = t.typeSymbol
      if ((ts.isParameter || (!ts.isClass && ts.isType && ts.isAbstract)) && !innerTparams.contains(ts)) {
        result += ts
      }
    }
    result.result()
  }

  def knownSubtypes(tpe: Type, ordered: Boolean = false): Option[List[Type]] = measure("knownSubtypes") {
    val dtpe = tpe.map(_.dealias)
    val tpeSym = dtpe match {
      case RefinedType(List(single), _) => single.typeSymbol
      case _ => dtpe.typeSymbol
    }

    def sort(subclasses: List[Symbol]): List[Symbol] =
      if (ordered || c.enclosingPosition.source == tpeSym.pos.source)
        subclasses.sortBy(positionPoint)
      else subclasses

    Option(tpeSym).filter(isSealedHierarchyRoot).map { sym =>
      sort(knownNonAbstractSubclasses(sym).toList)
        .flatMap(subSym => determineSubtype(dtpe, subSym.asType))
    }
  }

  // determine subtype assuming that we're dealing with simple TypeRefs (no structural types etc.)
  // and subtype simply passes its type parameters to base type without wrapping them into anything
  private def simplifiedDetermineSubtype(baseTpe: Type, subclass: TypeSymbol): Option[Type] =
    if (subclass.typeParams.isEmpty) Some(subclass.toType)
    else baseTpe match {
      case ExistentialType(quantified, underlying) =>
        simplifiedDetermineSubtype(underlying, subclass).map(internal.existentialAbstraction(quantified, _))
      case TypeRef(_, sym, baseArgs) => subclass.toType.baseType(sym) match {
        case TypeRef(_, _, undetBaseArgs) =>

          case class BaseArgMapping(baseTparam: TypeSymbol, baseArg: Type, undetSubBaseArg: Type)

          val tparamMapping = new mutable.HashMap[Symbol, Tree]
          val moreWildcards = new ListBuffer[MemberDef]

          def matchBaseArgs(baseArgs: List[BaseArgMapping]): Option[Type] = baseArgs match {
            case BaseArgMapping(basetp, arg, TypeRef(NoPrefix, s, Nil)) :: tail if subclass.typeParams.contains(s) =>
              val newArgTree =
                if (basetp.isCovariant && !s.asType.isCovariant || basetp.isContravariant && !s.asType.isContravariant) {
                  val bounds =
                    if (basetp.isCovariant) TypeBoundsTree(EmptyTree, TypeTree(arg))
                    else TypeBoundsTree(TypeTree(arg), EmptyTree)
                  moreWildcards += TypeDef(Modifiers(Flag.DEFERRED), s.name.toTypeName, Nil, bounds)
                  Ident(s.name)
                }
                else TypeTree(arg)

              tparamMapping(s) = newArgTree
              matchBaseArgs(tail)
            case Nil =>
              val subargs = subclass.typeParams.map(tp => tparamMapping.getOrElse(tp, Ident(tp)))
              val appliedSubclass = tq"$subclass[..$subargs]"
              val subtpeTree =
                if (moreWildcards.nonEmpty) ExistentialTypeTree(appliedSubclass, moreWildcards.result())
                else appliedSubclass

              val undetTparams = subclass.typeParams.filterNot(tparamMapping.contains)
              Some(internal.existentialAbstraction(undetTparams, getType(subtpeTree)))
            case _ => None
          }
          matchBaseArgs((sym.asType.typeParams zip baseArgs zip undetBaseArgs).map {
            case ((s, t), ut) => BaseArgMapping(s.asType, t, ut)
          })
      }
      case _ => None
    }

  def determineSubtype(baseTpe: Type, subclass: TypeSymbol): Option[Type] =
    simplifiedDetermineSubtype(baseTpe, subclass) orElse {
      val vname = c.freshName(TermName("v"))
      val normName = c.freshName(TermName("norm"))
      val tpref = c.freshName("tpref")
      val tparamBinds = subclass.typeParams.map(tp => Bind(TypeName(tpref + tp.name.toString), EmptyTree))
      val matchedTpe = AppliedTypeTree(treeForType(subclass.toTypeConstructor), tparamBinds)

      // Heavy wizardry employed to trick the compiler into performing the type computation that I need.
      // Nested macro call is required because _reasons_.
      // No, really, it's actually required because we're taking advantage of "GADT type refinement" performed
      // by the compiler when typechecking pattern matches. It involves some absolutely horrible mutation of
      // compiler-internal data structures (see `scala.tools.nsc.typechecker.Contexts.pushTypeBounds/restoreTypeBounds`
      // if you _really_ want to take a glimpse of it). As a consequence, the type that we want can only be accessed
      // while typechecking case body and not after that. Therefore we need a macro which will inject itself exactly
      // into that moment.
      val fakeMatch =
      q"""
          import scala.language.experimental.macros
          def $normName(tpref: $StringCls, value: $ScalaPkg.Any): $ScalaPkg.Any =
            macro $CommonsPkg.macros.misc.WhiteMiscMacros.normalizeGadtSubtype
          ($PredefObj.??? : $baseTpe) match {
            case $vname: $matchedTpe => $normName($tpref, $vname)
          }
         """

      c.typecheck(fakeMatch, silent = true) match {
        case EmptyTree => None
        case t => t.collect({ case Typed(Ident(`vname`), tpt) => tpt.tpe }).headOption
      }
    }

  def determineTypeParams(undetTpe: Type, detTpe: Type, typeParams: List[Symbol]): Option[List[Type]] = {
    val methodName = c.freshName(TermName("m"))
    val typeDefs = typeParams.map(typeSymbolToTypeDef(_, forMethod = true))

    val tree = typecheck(
      q"""
        def $methodName[..$typeDefs](f: ${treeForType(undetTpe)} => $UnitCls): $UnitCls = ()
        $methodName((_: $detTpe) => ())
      """, silent = true
    )

    tree match {
      case Block(_, Apply(TypeApply(_, args), _)) => Some(args.map(_.tpe))
      case Block(_, Apply(_, _)) => Some(Nil)
      case EmptyTree => None
    }
  }

  def abortOnTypecheckException[T](expr: => T): T =
    try expr catch {
      case TypecheckException(_, msg) => abort(msg)
    }

  def typecheckException(msg: String) =
    throw TypecheckException(c.enclosingPosition, msg)

  def isTuple(sym: Symbol): Boolean =
    definitions.TupleClass.seq.contains(sym)

  def isToplevelClass(s: Symbol): Boolean =
    s == definitions.AnyClass || s == definitions.ObjectClass || s == definitions.AnyValClass

  def isFromToplevelType(s: Symbol): Boolean =
    isToplevelClass(s.owner) || s.overrides.exists(ss => isToplevelClass(ss.owner))

  private class TparamRefStripper(tparams: List[Symbol]) extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case TypeTree() if tree.tpe != null && tree.tpe.exists(t => tparams.contains(t.typeSymbol)) =>
        treeForType(tree.tpe)
      case _ => super.transform(tree)
    }
  }

  def stripTparamRefs(tparams: List[Symbol])(tree: Tree): Tree =
    if (tparams.isEmpty) tree
    else new TparamRefStripper(tparams).transform(tree)
}
