package com.avsystem.commons
package macros.misc

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.reflect.macros.blackbox

/**
  * Author: ghik
  * Created: 11/12/15.
  */
class SealedMacros(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  import c.universe._

  def caseObjectsFor[T: c.WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    knownSubtypes(tpe).map { subtypes =>
      val objects = subtypes.map(subTpe => singleValueFor(subTpe)
        .getOrElse(abort(s"All possible values of a SealedEnum must be objects but $subTpe is not")))
      withKnownSubclassesCheck(q"$ListObj(..$objects)", tpe)
    }.getOrElse(abort(s"$tpe is not a sealed trait or class"))
  }
}
