package com.avsystem.commons
package macros.misc

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.reflect.macros.blackbox

class SamMacros(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  import c.universe._

  def validateSam[T: WeakTypeTag, F: WeakTypeTag]: Tree = {
    validateSam(weakTypeOf[T], weakTypeOf[F])
    q"null"
  }

  def createSam[T: WeakTypeTag](fun: Tree): Tree =
    createSam(weakTypeOf[T], fun.tpe, fun)

  def toSam[T: WeakTypeTag, F: WeakTypeTag](fun: Tree): Tree =
    createSam(weakTypeOf[T], weakTypeOf[F], fun)

  private def createSam(targetTpe: Type, funTpe: Type, fun: Tree): Tree = {
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

      val resultType = sig.finalResultType
      val defParamss = sig.paramLists.map(_.map(ps => {
        val implicitFlag = if (ps.isImplicit) Flag.IMPLICIT else NoFlags
        ValDef(Modifiers(Flag.PARAM | implicitFlag), ps.name.toTermName, TypeTree(ps.typeSignature), EmptyTree)
      }))

      val unimplemented = q"???"
      val baseResult = c.typecheck(
        q"""
          new $targetTpe {
            def ${m.name.toTermName}(...$defParamss): $resultType = $unimplemented
          }
         """)

      val typedDefParamss = baseResult
        .collect({ case dd: DefDef if dd.symbol.overrides.contains(m) => dd.vparamss })
        .head

      def rewriteParams(function: Tree, defParamss: List[List[ValDef]]): Tree =
        (function, defParamss) match {
          case (Function(funParams, body), defParams :: dpTail) =>
            val defParamByFunParam = (funParams.map(_.symbol) zip defParams).toMap
            object transformer extends Transformer {
              override def transform(tree: Tree) = tree match {
                case id: Ident if defParamByFunParam.contains(id.symbol) =>
                  val defParam = defParamByFunParam(id.symbol)
                  internal.setSymbol(treeCopy.Ident(id, defParam.name), defParam.symbol)
                case _ => super.transform(tree)
              }
            }
            rewriteParams(transformer.transform(body), dpTail)
          case (body, _) =>
            val paramss = defParamss.map(_.map(vd => Ident(vd.symbol)))
            c.typecheck(q"$body(...$paramss)")
        }

      val defBody = rewriteParams(fun, typedDefParamss)

      object transformer extends Transformer {
        override def transform(tree: Tree) = tree match {
          case DefDef(mods, name, tparams, vparamss, resultTpe, _) if tree.symbol.overrides.contains(m) =>
            treeCopy.DefDef(tree, mods, name, tparams, vparamss, resultTpe, defBody)
          case _ => super.transform(tree)
        }
      }

      transformer.transform(baseResult)
    }
  }

  private def validateSam(targetTpe: Type, funTpe: Type): Boolean = {
    val targetSymbol = targetTpe.dealias.typeSymbol
    if (!targetSymbol.isClass && !targetSymbol.asClass.isAbstract) {
      abort(s"$targetTpe is not a trait or abstract class")
    }

    targetTpe.members.iterator.filter(m => m.isAbstract).map(m => (m, m.typeSignatureIn(targetTpe))).toList match {
      case (m, sig) :: Nil if m.isPublic && m.isMethod && !m.asTerm.isAccessor && sig.typeParams.isEmpty =>
        val argTypess = sig.paramLists.map(_.map(_.typeSignature))
        val finalResultType = if (sig.finalResultType =:= typeOf[Unit]) typeOf[Any] else sig.finalResultType

        val requiredFunTpe = argTypess.foldRight(finalResultType) { (argTypes, resultType) =>
          val funSym = rootMirror.staticClass(s"_root_.scala.Function${argTypes.length}")
          internal.reificationSupport.TypeRef(NoPrefix, funSym, argTypes :+ resultType)
        }

        val emptyList = argTypess == List(Nil)
        val byName = emptyList && funTpe <:< finalResultType
        if (!byName && !(funTpe <:< requiredFunTpe)) {
          val requiredMsg = (if (emptyList) s"$finalResultType or " else "") + s"$requiredFunTpe"
          abort(s"$funTpe does not match signature of $m in $targetTpe: expected $requiredMsg")
        }
        byName
      case _ =>
        abort("Target trait/class must have exactly one public, abstract, non-generic method")
    }
  }
}
