package com.avsystem.commons
package macros.misc

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.reflect.macros.blackbox

class SealedMacros(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  import c.universe._

  val OrderedEnumType = getType(tq"$CommonsPackage.misc.OrderedEnum")

  def caseObjectsFor[T: c.WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    knownSubtypes(tpe).map { subtypes =>
      val objects = subtypes.map(subTpe => singleValueFor(subTpe)
        .getOrElse(abort(s"All possible values of a SealedEnum must be objects but $subTpe is not")))
      val result = q"$ListObj(..$objects)"
      if (tpe <:< OrderedEnumType) q"$result.sorted" else result
    }.getOrElse(abort(s"$tpe is not a sealed trait or class"))
  }
}
