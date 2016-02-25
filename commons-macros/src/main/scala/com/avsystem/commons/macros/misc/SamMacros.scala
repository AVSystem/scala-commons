package com.avsystem.commons
package macros.misc

import com.avsystem.commons.macros.MacroCommons

import scala.reflect.macros.blackbox

/**
  * Author: ghik
  * Created: 23/11/15.
  */
class SamMacros(val c: blackbox.Context) extends MacroCommons {

  import c.universe._

  def validateSam[T: c.WeakTypeTag, F: c.WeakTypeTag]: c.Tree = {
    validateSam(weakTypeOf[T], weakTypeOf[F])
    q"null"
  }

  def createSam[T: c.WeakTypeTag](fun: c.Expr[Any]): c.Tree =
    createSam(weakTypeOf[T], fun.actualType, fun.tree)

  def toSam[T: c.WeakTypeTag, F: c.WeakTypeTag](fun: c.Expr[F]): c.Tree =
    createSam(weakTypeOf[T], weakTypeOf[F], fun.tree)

  private def createSam(targetTpe: Type, funTpe: Type, fun: Tree): c.Tree = {
    val byName = validateSam(targetTpe, funTpe)
    val m = targetTpe.members.filter(m => m.isAbstract).head

    if (byName) {
      q"""
        new $targetTpe {
          def ${m.name.toTermName} = $fun
        }
       """
    } else {
      val sig = m.typeSignatureIn(targetTpe)

      val argNames = sig.paramLists.flatten.map(_.name.toTermName)
      val params = sig.paramLists.map(_.map(ps => {
        val implicitFlag = if (ps.isImplicit) Flag.IMPLICIT else NoFlags
        ValDef(Modifiers(Flag.PARAM | implicitFlag), ps.name.toTermName, TypeTree(ps.typeSignature), EmptyTree)
      }))

      val funName = c.freshName(TermName("fun"))
      q"""
        val $funName = $fun
        new $targetTpe {
          def ${m.name.toTermName}(...$params) = $funName(..$argNames)
        }
       """
    }
  }

  private def validateSam(targetTpe: Type, funTpe: Type): Boolean = {
    val targetSymbol = targetTpe.dealias.typeSymbol
    if (!targetSymbol.isClass && !targetSymbol.asClass.isAbstract) {
      abort(s"$targetTpe is not a trait or abstract class")
    }

    targetTpe.members.iterator.filter(m => m.isAbstract).map(m => (m, m.typeSignatureIn(targetTpe))).toList match {
      case (m, sig) :: Nil if m.isPublic && m.isMethod && !m.asTerm.isAccessor && sig.typeParams.isEmpty =>
        val argTypes = sig.paramLists.flatten.map(_.typeSignature)
        val arity = argTypes.length
        val resultType = if (sig.finalResultType =:= typeOf[Unit]) typeOf[Any] else sig.finalResultType

        val funSym = rootMirror.staticClass(s"_root_.scala.Function$arity")
        val requiredFunTpe = internal.reificationSupport.TypeRef(NoPrefix, funSym, argTypes :+ resultType)

        val byName = arity == 0 && funTpe <:< resultType
        if (!byName && !(funTpe <:< requiredFunTpe)) {
          val requiredMsg = (if (arity == 0) s"$resultType or " else "") + s"$requiredFunTpe"
          abort(s"$funTpe does not match signature of $m in $targetTpe: expected $requiredMsg")
        }
        byName
      case _ =>
        abort("Target trait/class must have exactly one public, abstract, non-generic method")
    }
  }
}
