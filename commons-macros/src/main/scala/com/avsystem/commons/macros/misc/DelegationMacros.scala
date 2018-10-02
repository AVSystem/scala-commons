package com.avsystem.commons
package macros.misc

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.reflect.macros.blackbox

class DelegationMacros(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  import c.universe._

  final def DelegationCls: Tree = tq"$MiscPkg.Delegation"

  def delegate[A: c.WeakTypeTag, B: c.WeakTypeTag](source: c.Tree): c.Tree = {
    val targetTpe = weakTypeOf[B]

    val targetSymbol = targetTpe.dealias.typeSymbol
    if (!targetSymbol.isClass && !targetSymbol.asClass.isAbstract) {
      abort(s"$targetTpe is not a trait or abstract class")
    }

    val wrappedName = c.freshName(TermName("w"))

    val methodDelegations = targetTpe.members.iterator
      .filter(m => m.isAbstract)
      .flatMap { m =>
        if (m.isPublic && m.isMethod && !m.asTerm.isSetter) {
          val ms = m.asMethod
          val name = m.name.toTermName
          val mtpe = m.typeSignatureIn(targetTpe)
          val typeNames = mtpe.typeParams.map(_.name.toTypeName)
          val typeDefs = mtpe.typeParams.map(typeSymbolToTypeDef(_))
          val paramNames = mtpe.paramLists.map(_.map(_.name.toTermName))
          val paramLists = mtpe.paramLists.map(_.map(paramSymbolToValDef))
          val resultTpeTree = treeForType(mtpe.finalResultType)

          val result = if (ms.isGetter)
            q"val $name: $resultTpeTree = $wrappedName.$name"
          else
            q"def $name[..$typeDefs](...$paramLists): $resultTpeTree = $wrappedName.$name[..$typeNames](...$paramNames)"

          Some(result)
        } else {
          error(s"Can't delegate ${m.name} - only public defs and vals can be delegated")
          None
        }
      }.toList

    q"""
      val $wrappedName = $source
      new $targetTpe {
        ..$methodDelegations
      }
     """
  }

  def materializeDelegation[A: c.WeakTypeTag, B: c.WeakTypeTag]: Tree = {
    val targetTpe = weakTypeOf[B]
    val sourceTpe = weakTypeOf[A]

    q"""
      new $DelegationCls[$sourceTpe, $targetTpe] {
        def delegate(source: $sourceTpe): $targetTpe = ${delegate[A, B](q"source")}
      }
     """
  }

}
