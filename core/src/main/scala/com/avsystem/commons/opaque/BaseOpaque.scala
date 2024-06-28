package com.avsystem.commons
package opaque

import com.avsystem.commons.opaque.Castable.<:>

private[opaque] trait BaseOpaque[From] extends Castable.Ops {
  trait Tag
  type Type

  implicit protected final val castable: From <:> Type = new Castable[From, Type]

  def apply(value: From): Type
}
