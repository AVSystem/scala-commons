package com.avsystem.commons
package macros.misc

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.reflect.macros.{TypecheckException, blackbox}

class MiscMacros(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  class C[+A, +B <: A]

  val aa: C[Any, Any] = new C[String, String]

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
      $CommonsPackage.misc.SourceInfo(
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
}
