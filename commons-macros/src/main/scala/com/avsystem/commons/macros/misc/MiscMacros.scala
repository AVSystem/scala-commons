package com.avsystem.commons
package macros.misc

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.reflect.macros.{TypecheckException, blackbox}

/**
  * Author: ghik
  * Created: 19/09/16.
  */
class MiscMacros(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  import c.universe._

  def infer[T: c.WeakTypeTag](clue: Tree): Tree =
    unwrapAndAddClue(clue)(c.typecheck(q"implicitly[${weakTypeOf[T]}]"))

  def inferNonMacro[T: c.WeakTypeTag](clue: Tree): Tree =
    unwrapAndAddClue(clue)(c.typecheck(q"implicitly[${weakTypeOf[T]}]", withMacrosDisabled = true))

  private def unwrapAndAddClue(clueTree: Tree)(expr: => Tree): Tree = clueTree match {
    case Literal(Constant(clue: String)) =>
      val wrapped = try expr catch {
        case TypecheckException(_, msg) => abort(clue + msg)
      }
      wrapped match {
        case Apply(_, List(arg)) => arg
        case tree => tree
      }
    case _ => abort(s"clue must be a String literal, $clueTree is not")
  }
}
