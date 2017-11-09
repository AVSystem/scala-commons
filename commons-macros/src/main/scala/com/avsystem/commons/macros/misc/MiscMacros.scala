package com.avsystem.commons
package macros.misc

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.reflect.macros.{TypecheckException, blackbox}

class MiscMacros(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  class C[+A, +B <: A]

  val aa: C[Any, Any] = new C[String, String]

  import c.universe._

  def infer[T: c.WeakTypeTag](clue: Tree): Tree =
    unwrapAndAddClue(clue)(c.typecheck(q"implicitly[${weakTypeOf[T]}]"))

  def inferNonMacro[T: c.WeakTypeTag](clue: Tree): Tree =
    unwrapAndAddClue(clue)(c.typecheck(q"implicitly[${weakTypeOf[T]}]", withMacrosDisabled = true))

  private def unwrapAndAddClue(clueTree: Tree)(expr: => Tree): Tree = clueTree match {
    case StringLiteral(clue) =>
      val wrapped = try expr catch {
        case TypecheckException(_, msg) => abort(clue + msg)
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
}
