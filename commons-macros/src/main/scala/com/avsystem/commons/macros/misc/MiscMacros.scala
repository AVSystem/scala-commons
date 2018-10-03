package com.avsystem.commons
package macros.misc

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.collection.mutable
import scala.reflect.macros.{blackbox, whitebox}
import scala.util.control.NoStackTrace

class MiscMacros(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  import c.universe._

  def infer[T: WeakTypeTag]: Tree =
    showOnDebug(inferTpe(weakTypeOf[T], "", NoPosition, withMacrosDisabled = false))

  def clueInfer[T: WeakTypeTag](clue: Tree): Tree =
    showOnDebug(inferTpe(weakTypeOf[T], clueStr(clue), clue.pos, withMacrosDisabled = false))

  def inferNonMacro[T: WeakTypeTag](clue: Tree): Tree =
    showOnDebug(inferTpe(weakTypeOf[T], clueStr(clue), clue.pos, withMacrosDisabled = true))

  private def clueStr(clue: Tree): String = clue match {
    case StringLiteral(str) => str
    case _ => abort(s"clue must be a String literal, $clue is not")
  }

  private def inferTpe(tpe: Type, clue: String, pos: Position, withMacrosDisabled: Boolean): Tree =
    c.inferImplicitValue(tpe, withMacrosDisabled = withMacrosDisabled) match {
      case EmptyTree => abortAt(clue + implicitNotFoundMsg(tpe), pos)
      case t => t
    }

  def sourceInfo: Tree = {
    def enclosingSymName(sym: Symbol) =
      sym.filter(_.isTerm).map(_.asTerm.getter).orElse(sym).name.decodedName.toString

    val pos = c.enclosingPosition
    q"""
      $MiscPkg.SourceInfo(
        ${pos.source.path},
        ${pos.source.file.name},
        ${pos.point},
        ${pos.line},
        ${pos.column},
        ${pos.source.lineToString(pos.line - 1)},
        $ListObj(..${ownerChain.takeWhile(_ != rootMirror.RootClass).map(enclosingSymName)})
      )
     """
  }

  def crossImpl(forJvm: Tree, forJs: Tree): Tree =
    if (isScalaJs) forJs else forJvm

  def enumValName: Tree = {
    def omitAnonClass(owner: Symbol): Symbol =
      if (owner.isConstructor && owner.owner.name.toString.contains("$anon"))
        owner.owner.owner
      else owner

    val owner = omitAnonClass(c.internal.enclosingOwner)
    val valid = owner.isTerm && owner.owner == c.prefix.tree.symbol && {
      val term = owner.asTerm
      term.isVal && term.isFinal && !term.isLazy && term.getter.isPublic &&
        term.typeSignature <:< getType(tq"${c.prefix}.Value")
    }
    if (!valid) {
      abort("ValueEnum must be assigned to a public, final, non-lazy val in its companion object " +
        "with explicit `Value` type annotation, e.g. `final val MyEnumValue: Value = new MyEnumClass")
    }

    q"new ${c.prefix}.ValName(${owner.asTerm.getter.name.decodedName.toString})"
  }

  def compilationError(error: Tree): Tree = error match {
    case StringLiteral(errorStr) =>
      abortAt(errorStr, error.pos)
    case t =>
      c.abort(t.pos, "Expected string literal here")
  }

  case class NonConcreteTypeException(tpe: Type) extends RuntimeException with NoStackTrace

  def javaClassName[T: WeakTypeTag]: Tree = showOnDebug {
    val tpe = weakTypeOf[T].dealias
    if (tpe.typeSymbol.isClass && tpe.typeSymbol != definitions.ArrayClass)
      q"new $MiscPkg.JavaClassName(${javaClassName(tpe.erasure.typeSymbol)})"
    else
      abort(s"$tpe does not represent a regular class")
  }

  private def javaClassName(sym: Symbol): String = {
    val nameSuffix = if (sym.isModuleClass && !sym.isPackageClass) "$" else ""
    val selfName = sym.name.encodedName.toString + nameSuffix
    val owner = sym.owner
    val prefix =
      if (owner == rootMirror.RootClass) ""
      else if (owner.isPackageClass) javaClassName(owner) + "."
      else if (owner.isModuleClass) javaClassName(owner)
      else javaClassName(owner) + "$"
    prefix + selfName
  }

  def typeString[T: WeakTypeTag]: Tree = showOnDebug {
    val tpe = weakTypeOf[T]
    try typeStringParts(tpe) match {
      case List(Select(pre, TermName("value"))) => pre
      case trees => q"new $MiscPkg.TypeString[$tpe](${mkStringConcat(trees)})"
    } catch {
      case NonConcreteTypeException(stpe) =>
        abort(s"Could not materialize TypeString for $tpe because instance for $stpe is lacking")
    }
  }

  private val allowedSymbols: mutable.Set[Symbol] = new mutable.HashSet

  def withAllowed[T](tparams: List[Symbol])(code: => T): T = {
    allowedSymbols ++= tparams
    try code finally {
      allowedSymbols --= tparams
    }
  }

  private def maybeParens(repr: List[Tree], parens: Boolean): List[Tree] =
    if (parens) lit("(") :: repr ::: lit(")") :: Nil
    else repr

  def typeStringParts(tpe: Type, parens: Boolean = false): List[Tree] = {
    val resultTpe = getType(tq"$MiscPkg.TypeString[$tpe]")
    c.inferImplicitValue(resultTpe, withMacrosDisabled = true) match {
      case EmptyTree => mkTypeString(tpe, parens)
      case tree => maybeParens(List(q"$tree.value"), parens)
    }
  }

  def mkStringConcat(trees: List[Tree]): Tree = trees match {
    case Nil => StringLiteral("")
    case single :: Nil => single
    case StringLiteral(str1) :: StringLiteral(str2) :: tail =>
      mkStringConcat(StringLiteral(str1 + str2) :: tail)
    case head :: tail =>
      q"$head+${mkStringConcat(tail)}"
  }

  def join(trees: List[List[Tree]], sep: String): List[Tree] = trees match {
    case Nil => Nil
    case List(single) => single
    case head :: tail => head ::: lit(sep) :: join(tail, sep)
  }

  def lit(str: String): Tree =
    StringLiteral(str)

  private def isOpChar(ch: Char): Boolean =
    ch != '`' && !ch.isLetterOrDigit

  private def isOpSafe(ch: Char): Boolean =
    ch == '.' || ch.isWhitespace

  def mkNameString(name: Name, prefix: String = "", suffix: String = ""): String = {
    val nameRepr = showCode(Ident(name))
    val afterPrefix = if (prefix.nonEmpty && !isOpSafe(prefix.last) && isOpChar(nameRepr.head)) " " else ""
    val beforeSuffix = if (suffix.nonEmpty && !isOpSafe(suffix.head) && isOpChar(nameRepr.last)) " " else ""
    s"$prefix$afterPrefix$nameRepr$beforeSuffix$suffix"
  }

  def isRefTo(quantified: Symbol, arg: Type): Boolean = arg match {
    case TypeRef(NoPrefix, `quantified`, Nil) => true
    case _ => false
  }

  def areIndependent(quantified: List[Symbol]): Boolean =
    quantified.forall(first => quantified.forall(second => !first.typeSignature.contains(second)))

  def mkMemberDefString(s: Symbol, wildcard: Boolean = false): List[Tree] = s match {
    case ExistentialSingleton(_, name, sig) =>
      lit(s"val ${mkNameString(name, suffix = ": ")}") :: mkTypeString(sig)
    case ts: TypeSymbol =>
      val variance = if (ts.isCovariant) "+" else if (ts.isContravariant) "-" else ""
      val beforeName = if (ts.isParameter) variance else "type "
      val finalPrefix = if (ts.isParameter) "" else " = "
      val baseDecl = if (wildcard) "_" else mkNameString(ts.name, prefix = beforeName)
      lit(baseDecl) :: mkSignatureString(ts.typeSignature, finalPrefix)
    case ts: TermSymbol =>
      val sig = ts.typeSignature
      val paramless = sig.typeParams.isEmpty && sig.paramLists.isEmpty
      val beforeName =
        if (ts.isParameter) ""
        else if (ts.isGetter && ts.setter != NoSymbol) "var "
        else if (ts.isGetter) "val "
        else "def "
      val baseDecl = mkNameString(ts.name, prefix = beforeName, suffix = if (paramless) ": " else "")
      lit(baseDecl) :: mkSignatureString(sig, if (paramless) "" else ": ")
  }

  def mkSignatureString(sig: Type, finalPrefix: String): List[Tree] = sig match {
    case TypeBounds(lo, hi) =>
      val loRepr =
        if (lo =:= typeOf[Nothing]) Nil
        else lit(" >: ") :: typeStringParts(lo)
      val hiRepr =
        if (hi =:= typeOf[Any]) Nil
        else lit(" <: ") :: typeStringParts(hi)
      loRepr ++ hiRepr
    case PolyType(tparams, resultType) => withAllowed(tparams) {
      lit("[") :: join(tparams.map(mkMemberDefString(_)), ", ") ::: lit("]") ::
        mkSignatureString(resultType, finalPrefix)
    }
    case MethodType(params, resultType) =>
      val pre = if (params.headOption.exists(_.isImplicit)) "(implicit " else "("
      lit(pre) :: join(params.map(mkMemberDefString(_)), ", ") ::: lit(")") ::
        withAllowed(params)(mkSignatureString(resultType, finalPrefix))
    case NullaryMethodType(resultType) =>
      mkSignatureString(resultType, finalPrefix)
    case _ =>
      lit(finalPrefix) :: mkTypeString(sig)
  }

  private val autoImported: Set[Symbol] = Set(
    definitions.ScalaPackage,
    definitions.JavaLangPackage,
    definitions.PredefModule,
    rootMirror.RootPackage
  )

  def isStaticPrefix(pre: Type): Boolean = pre match {
    case SingleType(ppre, _) => isStaticPrefix(ppre)
    case ThisType(sym) => sym.isPublic && sym.isStatic && sym.isModuleClass
    case TypeRef(ppre, sym, Nil) => sym.isPublic && sym.isStatic && sym.isModuleClass && isStaticPrefix(ppre)
    case _ => false
  }

  def isAllowedWithoutPrefix(s: Symbol): Boolean =
    (s.isType && s.asType.isAliasType) || allowedSymbols.contains(s)

  def mkTypeString(tpe: Type, parens: Boolean = false): List[Tree] = tpe match {
    case _ if tpe =:= typeOf[AnyRef] => List(lit("AnyRef"))
    case TypeRef(NoPrefix, ExistentialSingleton(_, name, _), Nil) =>
      List(lit(mkNameString(name)))
    case TypeRef(_, sym, List(arg)) if sym == definitions.ByNameParamClass =>
      val res = lit("=> ") :: typeStringParts(arg)
      maybeParens(res, parens)
    case TypeRef(_, sym, List(arg)) if sym == definitions.RepeatedParamClass =>
      val argRepr = typeStringParts(arg)
      val starSafe = argRepr.last match {
        case StringLiteral(s) => s.charAt(s.length - 1) match {
          case ']' | ')' | '}' | '`' => true
          case ch => ch.isLetterOrDigit
        }
        case _ => false
      }
      argRepr ::: lit(if (starSafe) "*" else " *") :: Nil
    case TypeRef(_, sym, args) if definitions.FunctionClass.seq.contains(sym) =>
      val fargs = args.init
      val fres = args.last
      val fargsRes = fargs match {
        case Nil => List(lit("()"))
        case List(single) => typeStringParts(single, parens = true)
        case _ => maybeParens(join(fargs.map(typeStringParts(_)), ", "), parens = true)
      }
      val res = fargsRes ::: lit(" => ") :: typeStringParts(fres)
      maybeParens(res, parens)
    case TypeRef(_, sym, args) if definitions.TupleClass.seq.contains(sym) =>
      lit("(") :: join(args.map(typeStringParts(_)), ", ") ::: lit(")") :: Nil
    case TypeRef(pre, sym, args) if pre != NoPrefix || isAllowedWithoutPrefix(sym) =>
      val dealiased = tpe.dealias
      if (dealiased.typeSymbol != sym && !isStaticPrefix(pre))
        mkTypeString(dealiased)
      else {
        val argsReprs =
          if (args.isEmpty) Nil
          else lit("[") +: join(args.map(typeStringParts(_)), ", ") :+ lit("]")
        mkTypePath(pre, sym.name) ::: argsReprs
      }
    case SingleType(pre, sym) if pre != NoPrefix || isAllowedWithoutPrefix(sym) =>
      mkTypePath(pre, sym.name) :+ lit(".type")
    case ThisType(sym) if sym.isStatic && sym.isModuleClass =>
      List(lit(mkStaticPrefix(sym) + "type"))
    case ExistentialType(quantified, TypeRef(pre, sym, args))
      if quantified.corresponds(args)(isRefTo) && quantified.forall(s => !pre.contains(s)) && areIndependent(quantified) =>
      withAllowed(quantified) {
        val wildcards = lit("[") +: join(quantified.map(mkMemberDefString(_, wildcard = true)), ", ") :+ lit("]")
        mkTypePath(pre, sym.name) ::: wildcards
      }
    case ExistentialType(quantified, underlying) =>
      withAllowed(quantified) {
        val typeDefs = join(quantified.map(mkMemberDefString(_)), "; ")
        maybeParens(typeStringParts(underlying) ::: lit(" forSome {") :: typeDefs ::: lit("}") :: Nil, parens)
      }
    case RefinedType(bases, scope) =>
      val basesRepr = bases match {
        case List(anyref) if anyref =:= typeOf[AnyRef] => Nil
        case _ => join(bases.map(typeStringParts(_)), " with ")
      }
      val filteredScope = scope.iterator.filter(s => !s.isTerm || !s.asTerm.isSetter).toList
      val scopeRepr =
        if (filteredScope.isEmpty) Nil
        else {
          val memberDefs = withAllowed(List(tpe.typeSymbol))(join(filteredScope.map(mkMemberDefString(_)), "; "))
          lit("{") :: memberDefs ::: lit("}") :: Nil
        }
      val space = if (basesRepr.nonEmpty && scopeRepr.nonEmpty) " " else ""
      maybeParens(basesRepr ::: lit(space) :: scopeRepr, parens)
    case AnnotatedType(_, underlying) =>
      mkTypeString(underlying)
    case _ =>
      throw NonConcreteTypeException(tpe)
  }

  def mkStaticPrefix(sym: Symbol): String =
    if (sym == rootMirror.RootClass) ""
    else mkStaticPrefix(sym.owner) + mkNameString(sym.name) + "."

  def mkTypePath(pre: Type, name: Name): List[Tree] = pre match {
    case t if autoImported.contains(t.termSymbol) =>
      List(lit(mkNameString(name)))
    case NoPrefix =>
      List(lit(mkNameString(name)))
    case SingleType(pkg, sym) if sym.name == termNames.PACKAGE && pkg.typeSymbol.isPackageClass =>
      mkTypePath(pkg, name)
    case SingleType(ppre, sym) if ppre != NoPrefix || isAllowedWithoutPrefix(sym) =>
      mkTypePath(ppre, sym.name) ::: lit("." + mkNameString(name)) :: Nil
    case ThisType(sym) if sym.isStatic && sym.isModuleClass =>
      List(lit(mkStaticPrefix(sym) + mkNameString(name)))
    case ThisType(sym) if allowedSymbols.contains(sym) =>
      List(lit(mkNameString(name)))
    case TypeRef(NoPrefix, ExistentialSingleton(_, valName, _), Nil) =>
      List(lit(mkNameString(valName) + "." + mkNameString(name)))
    case _ =>
      val singletonPrefix = pre.typeSymbol.isModuleClass || pre.termSymbol != NoSymbol
      val selection = if (singletonPrefix) "." else "#"
      mkTypeString(pre, parens = !singletonPrefix) :+ lit(mkNameString(name, prefix = selection))
  }

  def lazyMetadata(metadata: Tree): Tree =
    q"${c.prefix}($metadata)"

  def mkValueOf[T: WeakTypeTag]: Tree = showOnDebug {
    val tpe = weakTypeOf[T].dealias
    singleValueFor(tpe) match {
      case Some(sv) => q"new $MiscPkg.ValueOf[$tpe]($sv)"
      case None => abort(s"$tpe is not a singleton type")
    }
  }

  def macroInstances: Tree = {
    val resultTpe = c.macroApplication.tpe
    val applySig = resultTpe.member(TermName("apply")).typeSignatureIn(resultTpe)
    val implicitsTpe = applySig.paramLists.head.head.typeSignature
    val instancesTpe = applySig.finalResultType

    val instTs = instancesTpe.typeSymbol
    if (!(instTs.isClass && instTs.isAbstract)) {
      abort(s"Expected trait or abstract class type, got $instancesTpe")
    }

    val instancesMethods = instancesTpe.members.iterator
      .filter(m => m.isAbstract && m.isMethod).toList.reverse

    def impl(singleMethod: Option[Symbol]): Tree = {
      val impls = instancesMethods.map { m =>
        val sig = m.typeSignatureIn(instancesTpe)
        val resultTpe = sig.finalResultType.dealias
        if (sig.typeParams.nonEmpty || sig.paramLists.nonEmpty) {
          abort(s"Problem with $m of $instancesTpe: expected non-generic, parameterless method")
        }
        val resultCompanion = typedCompanionOf(resultTpe)
          .getOrElse(abort(s"$resultTpe has no companion object with `materialize` macro"))

        val body =
          if (singleMethod.exists(_ != m)) q"$PredefObj.???"
          else q"$resultCompanion.materialize"

        q"def ${m.name.toTermName} = $body"
      }

      val implicitsName = c.freshName(TermName("implicits"))

      q"""
        new $resultTpe {
          def apply($implicitsName: $implicitsTpe, $companionReplacementName: Any): $instancesTpe = {
            import $implicitsName._
            new $instancesTpe { ..$impls; () }
          }
        }
       """
    }

    //If full implementation doesn't typecheck, find the first problematic typeclass and limit
    //compilation errors to that one in order to not overwhelm the user but rather report errors gradually
    val fullImpl = impl(None)
    c.typecheck(fullImpl, silent = true) match {
      case EmptyTree =>
        instancesMethods.iterator.map(m => impl(Some(m)))
          .find(t => c.typecheck(t, silent = true) == EmptyTree)
          .getOrElse(fullImpl)
      case t => t
    }
  }

  def posPoint: Tree =
    q"${c.enclosingPosition.point}"

  def applyUnapplyOrFail(tpe: Type): ApplyUnapply =
    applyUnapplyFor(tpe).getOrElse(abort(
      s"$tpe is not a case class or case-class like type: no matching apply/unapply pair found"))

  def applyBody(rawValuesName: TermName, tpe: Type, au: ApplyUnapply): Tree = {
    val args = au.params.zipWithIndex.map { case ((param, _), idx) =>
      val res = q"$rawValuesName($idx).asInstanceOf[${actualParamType(param.typeSignature)}]"
      if (isRepeated(param)) q"$res: _*" else res
    }
    if (au.standardCaseClass) q"new $tpe(..$args)"
    else q"${replaceCompanion(typedCompanionOf(tpe).getOrElse(EmptyTree))}.apply[..${tpe.typeArgs}](..$args)"
  }

  def unapplyBody(valueName: TermName, tpe: Type, au: ApplyUnapply): Tree = {
    if (au.standardCaseClass) q"$ScalaPkg.Array(..${au.params.map { case (param, _) => q"$valueName.$param" }})"
    else {
      val safeCompanion = replaceCompanion(typedCompanionOf(tpe).getOrElse(EmptyTree))
      val unapplyRes = q"$safeCompanion.${au.unapply}[..${tpe.typeArgs}]($valueName)"
      au.params match {
        case Nil => q"$ScalaPkg.Seq.empty[$ScalaPkg.Any]"
        case List(_) => q"$ScalaPkg.Array($unapplyRes.get)"
        case _ =>
          val resName = c.freshName(TermName("res"))
          val elems = au.params.indices.map(i => q"$resName.${TermName(s"_${i + 1}")}")
          q"""
             val $resName = $unapplyRes.get
             $ScalaPkg.Array[$ScalaPkg.Any](..$elems)
           """
      }
    }
  }

  def applier[T: WeakTypeTag]: Tree = showOnDebug {
    val tpe = weakTypeOf[T].dealias
    val rawValuesName = c.freshName(TermName("rawValues"))
    q"""
      new $MiscPkg.Applier[$tpe] {
        def apply($rawValuesName: $ScalaPkg.Seq[$ScalaPkg.Any]): $tpe =
          ${applyBody(rawValuesName, tpe, applyUnapplyOrFail(tpe))}
      }
    """
  }

  def unapplier[T: WeakTypeTag]: Tree = showOnDebug {
    val tpe = weakTypeOf[T].dealias
    val valueName = c.freshName(TermName("value"))
    val au = applyUnapplyOrFail(tpe)
    if (au.standardCaseClass && tpe <:< ProductTpe)
      q"new $MiscPkg.ProductUnapplier[$tpe]"
    else
      q"""
        new $MiscPkg.Unapplier[$tpe] {
          def unapply($valueName: $tpe): $ScalaPkg.Seq[$ScalaPkg.Any] =
            ${unapplyBody(valueName, tpe, au)}
        }
      """
  }

  def applierUnapplier[T: WeakTypeTag]: Tree = showOnDebug {
    val tpe = weakTypeOf[T].dealias
    val rawValuesName = c.freshName(TermName("rawValues"))
    val valueName = c.freshName(TermName("value"))
    val au = applyUnapplyOrFail(tpe)
    if (au.standardCaseClass && tpe <:< ProductTpe)
      q"""
        new $MiscPkg.ProductApplierUnapplier[$tpe] {
          def apply($rawValuesName: $ScalaPkg.Seq[$ScalaPkg.Any]): $tpe =
            ${applyBody(rawValuesName, tpe, au)}
        }
       """
    else
      q"""
        new $MiscPkg.ApplierUnapplier[$tpe] {
          def apply($rawValuesName: $ScalaPkg.Seq[$ScalaPkg.Any]): $tpe =
            ${applyBody(rawValuesName, tpe, au)}
          def unapply($valueName: $tpe): $ScalaPkg.Seq[$ScalaPkg.Any] =
            ${unapplyBody(valueName, tpe, au)}
        }
      """
  }

  def assertLocal(tpe: Type): Type = {
    if (tpe.typeSymbol.pos.source != c.enclosingPosition.source) {
      abort(s"Macro inspection of $tpe can only be done in the same source file where that type is defined")
    }
    tpe
  }

  def safeAnnotTree(annot: Annot): Tree = {
    if (containsInaccessibleThises(annot.tree)) {
      abortAt(s"Reified annotation ${annot.tree} contains inaccessible this-references", annot.tree.pos)
    }
    c.untypecheck(annot.tree)
  }

  def annotationOf[A: WeakTypeTag, T: WeakTypeTag]: Tree = {
    val atpe = weakTypeOf[A].dealias
    val tpe = assertLocal(weakTypeOf[T].dealias)
    val annot = findAnnotation(tpe.typeSymbol, atpe)
      .getOrElse(abort(s"No annotation of type $atpe found on $tpe"))
    q"$MiscPkg.AnnotationOf(${safeAnnotTree(annot)})"
  }

  def optAnnotationOf[A: WeakTypeTag, T: WeakTypeTag]: Tree = {
    val atpe = weakTypeOf[A].dealias
    val tpe = assertLocal(weakTypeOf[T].dealias)
    val annotTree = findAnnotation(tpe.typeSymbol, atpe)
      .fold[Tree](q"$MiscPkg.Opt.Empty")(a => q"$MiscPkg.Opt(${safeAnnotTree(a)})")
    q"$MiscPkg.OptAnnotationOf($annotTree)"
  }

  def annotationsOf[A: WeakTypeTag, T: WeakTypeTag]: Tree = {
    val atpe = weakTypeOf[A].dealias
    val tpe = assertLocal(weakTypeOf[T].dealias)
    val annots = allAnnotations(tpe.typeSymbol, atpe).map(safeAnnotTree)
    q"$MiscPkg.AnnotationsOf($ListObj(..$annots))"
  }
}

class WhiteMiscMacros(ctx: whitebox.Context) extends AbstractMacroCommons(ctx) {

  import c.universe._

  final lazy val WhenAbsentAT: Type = getType(tq"$CommonsPkg.serialization.whenAbsent[_]")

  def whenAbsentValue: Tree = {
    val param = c.internal.enclosingOwner match {
      case DefaultValueMethod(p) => p
      case p => p
    }
    findAnnotation(param, WhenAbsentAT).map(_.tree).map {
      case Apply(_, List(MaybeTyped(arg, _))) => arg
      case t => abort(s"unexpected tree for @whenAbsent annotation: $t")
    } getOrElse {
      abort(s"no @whenAbsent annotation found on $param of ${param.owner}")
    }
  }

  def inferValue: Tree = {
    val param = c.internal.enclosingOwner match {
      case DefaultValueMethod(p) => p
      case p => p
    }
    if (param.owner.owner.asType.toType <:< AnnotationTpe && findAnnotation(param, InferAT).nonEmpty)
      q"""throw new $ScalaPkg.NotImplementedError("infer.value")"""
    else
      abort(s"infer.value can be only used as default value of @infer annotation parameters")
  }
}
