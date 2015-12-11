package com.avsystem.commons
package misc

/**
  * Author: ghik
  * Created: 11/12/15.
  */
object SealedUtils {
  def caseObjectsFor[T]: List[T] = macro macros.misc.SealedMacros.caseObjectsFor[T]
}

trait SealedEnumCompanion[T] {
  def values: List[T]

  lazy val byName = values.iterator.map(t => (t.toString, t)).toMap
}
