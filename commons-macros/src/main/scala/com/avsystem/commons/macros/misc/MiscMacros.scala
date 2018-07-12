package com.avsystem.commons
package macros.misc

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.reflect.macros.{TypecheckException, blackbox}

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

  def typeString[T: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    typeStringParts(tpe) match {
      case List(Select(pre, TermName("value"))) => pre
      case trees => q"$CommonsPkg.misc.TypeString[$tpe](${mkStringConcat(trees)})"
    }
  }

  def typeStringParts(tpe: Type): List[Tree] = {
    val resultTpe = getType(tq"$CommonsPkg.misc.TypeString[$tpe]")
    c.inferImplicitValue(resultTpe, withMacrosDisabled = true) match {
      case EmptyTree => mkTypeString(tpe)
      case tree => List(q"$tree.value")
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

  def mkNameString(name: Name): String =
    showCode(Ident(name))

  def isRefTo(quantified: Symbol, arg: Type): Boolean = arg match {
    case TypeRef(NoPrefix, `quantified`, Nil) => true
    case _ => false
  }

  def areIndependent(quantified: List[Symbol]): Boolean =
    quantified.forall(first => quantified.forall(second => !first.typeSignature.contains(second)))

  def mkTypeDefString(s: Symbol, wildcard: Boolean): List[Tree] = s match {
    case ExistentialSingleton(_, name, sig) =>
      lit(s"val ${mkNameString(name)}: ") :: mkTypeString(sig)
    case _ =>
      lit(if (wildcard) "_" else s"type ${mkNameString(s.name)}") :: mkTypeString(s.typeSignature)
  }

  private val autoImported: Set[Symbol] = Set(
    definitions.ScalaPackage,
    definitions.JavaLangPackage,
    definitions.PredefModule
  )

  def isStaticPrefix(pre: Type): Boolean = pre match {
    case SingleType(ppre, _) => isStaticPrefix(ppre)
    case ThisType(sym) => sym.isStatic && sym.isModuleClass
    case TypeRef(ppre, sym, Nil) => sym.isStatic && sym.isModuleClass && isStaticPrefix(ppre)
    case _ => false
  }

  def mkTypeString(tpe: Type): List[Tree] = tpe match {
    case _ if tpe =:= typeOf[AnyRef] => List(lit("AnyRef"))
    case TypeRef(NoPrefix, ExistentialSingleton(_, name, _), Nil) =>
      List(lit(mkNameString(name)))
    case TypeRef(_, sym, args) if definitions.FunctionClass.seq.contains(sym) =>
      val fargs = args.init
      val fres = args.last
      lit("(") :: join(fargs.map(typeStringParts), ", ") ::: lit(") => ") :: typeStringParts(fres)
    case TypeRef(_, sym, args) if definitions.TupleClass.seq.contains(sym) =>
      lit("(") :: join(args.map(typeStringParts), ", ") ::: lit(")") :: Nil
    case TypeRef(pre, sym, args) if !sym.isParameter =>
      val dealiased = tpe.dealias
      if (dealiased.typeSymbol != sym && !isStaticPrefix(pre)) {
        mkTypeString(dealiased)
      } else {
        val argsReprs =
          if (args.isEmpty) Nil
          else lit("[") +: join(args.map(typeStringParts), ", ") :+ lit("]")
        mkTypePrefix(pre) ::: lit(mkNameString(sym.name)) :: argsReprs
      }
    case SingleType(_, _) =>
      mkTypePrefix(tpe) :+ lit("type")
    case ThisType(sym) if sym.isStatic && sym.isModuleClass =>
      List(lit(mkStaticPrefix(sym) + "type"))
    case ExistentialType(quantified, TypeRef(pre, sym, args))
      if quantified.corresponds(args)(isRefTo) && quantified.forall(s => !pre.contains(s)) && areIndependent(quantified) =>
      val wildcards = lit("[") +: join(quantified.map(mkTypeDefString(_, wildcard = true)), ", ") :+ lit("]")
      mkTypePrefix(pre) ::: lit(mkNameString(sym.name)) :: wildcards
    case ExistentialType(quantified, underlying) =>
      val typeDefs = join(quantified.map(mkTypeDefString(_, wildcard = false)), "; ")
      typeStringParts(underlying) ::: lit(" forSome {") :: typeDefs ::: lit("}") :: Nil
    case TypeBounds(lo, hi) =>
      val loRepr =
        if (lo =:= typeOf[Nothing]) Nil
        else lit(" >: ") :: typeStringParts(lo)
      val hiRepr =
        if (hi =:= typeOf[Any]) Nil
        else lit(" <: ") :: typeStringParts(hi)
      loRepr ++ hiRepr
    case RefinedType(bases, scope) if scope.forall(_.isType) =>
      val basesRepr = join(bases.map(typeStringParts), " with ")
      val scopeRepr =
        if (scope.isEmpty) Nil
        else lit(" {") :: join(scope.map(mkTypeDefString(_, wildcard = false)).toList, "; ") ::: lit("}") :: Nil
      basesRepr ++ scopeRepr
    case _ =>
      abort(s"Could not find nor materialize TypeString for $tpe")
  }

  def mkStaticPrefix(sym: Symbol): String =
    if (sym == rootMirror.RootClass) ""
    else mkStaticPrefix(sym.owner) + mkNameString(sym.name) + "."

  def mkTypePrefix(tpe: Type): List[Tree] = tpe match {
    case t if autoImported.contains(t.termSymbol) => Nil
    case NoPrefix => Nil
    case SingleType(pkg, sym) if sym.name == termNames.PACKAGE && pkg.typeSymbol.isPackageClass => mkTypePrefix(pkg)
    case SingleType(pre, sym) => mkTypePrefix(pre) :+ lit(mkNameString(sym.name) + ".")
    case ThisType(sym) if sym.isStatic && sym.isModuleClass => List(lit(mkStaticPrefix(sym)))
    case TypeRef(NoPrefix, ExistentialSingleton(_, name, _), Nil) => List(lit(mkNameString(name) + "."))
    case _ => mkTypeString(tpe) :+ lit(if (tpe.typeSymbol.isModuleClass || tpe.termSymbol != NoSymbol) "." else "#")
  }
}
