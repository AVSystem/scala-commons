package com.avsystem.commons
package macros.misc

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.collection.mutable
import scala.reflect.macros.{TypecheckException, blackbox}
import scala.util.control.NoStackTrace

class MiscMacros(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  import c.universe._

  // cannot use c.inferImplicitValue(silent = false) because the only error it reports is
  // "implicit search has failed, to find out the reason turn -Xlog-implicits"
  def infer[T: c.WeakTypeTag](clue: Tree): Tree =
    unwrapAndAddClue(clue)(c.typecheck(q"implicitly[${weakTypeOf[T]}]"))

  def inferNonMacro[T: c.WeakTypeTag](clue: Tree): Tree =
    unwrapAndAddClue(clue)(c.typecheck(q"implicitly[${weakTypeOf[T]}]", withMacrosDisabled = true))

  private def unwrapAndAddClue(clueTree: Tree)(expr: => Tree): Tree = clueTree match {
    case StringLiteral(clue) =>
      val wrapped = try expr catch {
        case TypecheckException(_, msg) => abortAt(clue + msg, clueTree.pos)
      }
      wrapped match {
        case Apply(_, List(arg)) => arg
        case tree => tree
      }
    case _ => abort(s"clue must be a String literal, $clueTree is not")
  }

  def sourceInfo: Tree = {
    def enclosingSymName(sym: Symbol) =
      sym.filter(_.isTerm).map(_.asTerm.getter).orElse(sym).name.decodedName.toString

    val pos = c.enclosingPosition
    q"""
      $CommonsPkg.misc.SourceInfo(
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

  def javaClassName[T: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T].dealias
    if (tpe.typeSymbol.isClass && tpe.typeSymbol != definitions.ArrayClass)
      q"new $CommonsPkg.misc.JavaClassName(${javaClassName(tpe.erasure.typeSymbol)})"
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

  def typeString[T: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    try typeStringParts(tpe) match {
      case List(Select(pre, TermName("value"))) => pre
      case trees => q"new $CommonsPkg.misc.TypeString[$tpe](${mkStringConcat(trees)})"
    } catch {
      case NonConcreteTypeException(stpe) =>
        abort(s"Could not materialize TypeString for $tpe because instance for $stpe is lacking")
    }
  }

  private val allowedTparams: mutable.Set[Symbol] = new mutable.HashSet

  def withAllowedTparams[T](tparams: List[Symbol])(code: => T): T = {
    allowedTparams ++= tparams
    try code finally {
      allowedTparams --= tparams
    }
  }

  private def maybeParens(repr: List[Tree], parens: Boolean): List[Tree] =
    if (parens) lit("(") :: repr ::: lit(")") :: Nil
    else repr

  def typeStringParts(tpe: Type, parens: Boolean = false): List[Tree] = {
    val resultTpe = getType(tq"$CommonsPkg.misc.TypeString[$tpe]")
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
    ch != '`' && !ch.isLetter

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

  def mkTypeDefString(s: Symbol, wildcard: Boolean = false): List[Tree] = s match {
    case ExistentialSingleton(_, name, sig) =>
      lit(s"val ${mkNameString(name, suffix = ": ")}") :: mkTypeString(sig)
    case _ =>
      val ts = s.asType
      val variance = if (ts.isCovariant) "+" else if (ts.isContravariant) "-" else ""
      val beforeName = if (ts.isParameter) variance else "type "
      val baseDecl = if (wildcard) "_" else mkNameString(ts.name, prefix = beforeName)
      lit(baseDecl) :: withAllowedTparams(List(ts))(mkTypeDefSigString(ts.typeSignature))
  }

  def mkTypeDefSigString(sig: Type): List[Tree] = sig match {
    case TypeBounds(lo, hi) =>
      val loRepr =
        if (lo =:= typeOf[Nothing]) Nil
        else lit(" >: ") :: typeStringParts(lo)
      val hiRepr =
        if (hi =:= typeOf[Any]) Nil
        else lit(" <: ") :: typeStringParts(hi)
      loRepr ++ hiRepr
    case PolyType(tparams, resultType) =>
      lit("[") :: join(tparams.map(mkTypeDefString(_)), ", ") ::: lit("]") ::
        withAllowedTparams(tparams)(mkTypeDefSigString(resultType))
    case _ =>
      lit(" = ") :: mkTypeString(sig)
  }

  private val autoImported: Set[Symbol] = Set(
    definitions.ScalaPackage,
    definitions.JavaLangPackage,
    definitions.PredefModule
  )

  def isStaticPrefix(pre: Type): Boolean = pre match {
    case SingleType(ppre, _) => isStaticPrefix(ppre)
    case ThisType(sym) => sym.isPublic && sym.isStatic && sym.isModuleClass
    case TypeRef(ppre, sym, Nil) => sym.isPublic && sym.isStatic && sym.isModuleClass && isStaticPrefix(ppre)
    case _ => false
  }

  def mkTypeString(tpe: Type, parens: Boolean = false): List[Tree] = tpe match {
    case _ if tpe =:= typeOf[AnyRef] => List(lit("AnyRef"))
    case TypeRef(NoPrefix, ExistentialSingleton(_, name, _), Nil) =>
      List(lit(mkNameString(name)))
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
    case TypeRef(pre, sym, args) if !sym.isParameter || allowedTparams.contains(sym) =>
      val dealiased = tpe.dealias
      if (dealiased.typeSymbol != sym && !isStaticPrefix(pre))
        mkTypeString(dealiased)
      else {
        val argsReprs =
          if (args.isEmpty) Nil
          else lit("[") +: join(args.map(typeStringParts(_)), ", ") :+ lit("]")
        mkTypePath(pre, sym.name) ::: argsReprs
      }
    case SingleType(pre, sym) =>
      mkTypePath(pre, sym.name) :+ lit(".type")
    case ThisType(sym) if sym.isStatic && sym.isModuleClass =>
      List(lit(mkStaticPrefix(sym) + "type"))
    case ExistentialType(quantified, TypeRef(pre, sym, args))
      if quantified.corresponds(args)(isRefTo) && quantified.forall(s => !pre.contains(s)) && areIndependent(quantified) =>
      val wildcards = lit("[") +: join(quantified.map(mkTypeDefString(_, wildcard = true)), ", ") :+ lit("]")
      mkTypePath(pre, sym.name) ::: wildcards
    case ExistentialType(quantified, underlying) =>
      val typeDefs = join(quantified.map(mkTypeDefString(_)), "; ")
      maybeParens(typeStringParts(underlying) ::: lit(" forSome {") :: typeDefs ::: lit("}") :: Nil, parens)
    case RefinedType(bases, scope) if scope.forall(_.isType) =>
      val basesRepr = join(bases.map(typeStringParts(_)), " with ")
      val scopeRepr =
        if (scope.isEmpty) Nil
        else lit(" {") :: join(scope.map(mkTypeDefString(_)).toList, "; ") ::: lit("}") :: Nil
      maybeParens(basesRepr ++ scopeRepr, parens)
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
    case SingleType(ppre, sym) =>
      mkTypePath(ppre, sym.name) ::: lit("." + mkNameString(name)) :: Nil
    case ThisType(sym) if sym.isStatic && sym.isModuleClass =>
      List(lit(mkStaticPrefix(sym) + mkNameString(name)))
    case TypeRef(NoPrefix, ExistentialSingleton(_, valName, _), Nil) =>
      List(lit(mkNameString(valName) + "." + mkNameString(name)))
    case _ =>
      val singletonPrefix = pre.typeSymbol.isModuleClass || pre.termSymbol != NoSymbol
      val selection = if (singletonPrefix) "." else "#"
      mkTypeString(pre, parens = !singletonPrefix) :+ lit(mkNameString(name, prefix = selection))
  }
}
