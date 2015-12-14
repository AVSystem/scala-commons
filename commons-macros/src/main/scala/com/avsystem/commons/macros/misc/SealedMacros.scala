package com.avsystem.commons
package macros.misc

import com.avsystem.commons.macros.MacroCommons

import scala.reflect.macros.blackbox

/**
  * Author: ghik
  * Created: 11/12/15.
  */
class SealedMacros(val c: blackbox.Context) extends MacroCommons {

  import c.universe._

  def caseObjectsFor[T: c.WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    knownSubtypes(tpe).map { subtypes =>
      val objects = subtypes.flatMap(singleValueFor)
      q"$ListObj(..$objects)"
    }.getOrElse(abort(s"$tpe is not a sealed trait or class"))
  }
}
