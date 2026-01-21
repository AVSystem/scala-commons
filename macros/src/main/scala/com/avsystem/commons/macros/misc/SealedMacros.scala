package com.avsystem.commons
package macros.misc

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.reflect.macros.blackbox

class SealedMacros(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  import c.universe.*

  final lazy val OrderedEnumType: Type = staticType(tq"$MiscPkg.OrderedEnum")

  def caseObjectsFor[T: WeakTypeTag]: Tree = instrument {
    val tpe = weakTypeOf[T]
    knownSubtypes(tpe)
      .map { subtypes =>
        val objects = subtypes.map(subTpe =>
          singleValueFor(subTpe)
            .getOrElse(abort(s"All possible values of a SealedEnum must be objects but $subTpe is not")),
        )
        val result = q"$ListObj(..$objects)"
        if (tpe <:< OrderedEnumType) q"$result.sorted" else result
      }
      .getOrElse(abort(s"$tpe is not a sealed trait or class"))
  }

  def instancesFor[TC: WeakTypeTag, T: WeakTypeTag]: Tree = instrument {
    val tpe = weakTypeOf[T]
    def instanceTpe(forTpe: Type): Type = weakTypeOf[TC] match {
      case TypeRef(pre, sym, Nil) => internal.typeRef(pre, sym, List(forTpe))
      case _ => abort(s"expected type constructor")
    }
    val subtypes = knownSubtypes(tpe).getOrElse(abort(s"$tpe is not a sealed hierarchy root"))
    q"${subtypes.map(st => q"""$ImplicitsObj.infer[${instanceTpe(st)}]("")""")}"
  }
}
