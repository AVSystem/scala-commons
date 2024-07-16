package com.avsystem.commons
package opaque

import com.avsystem.commons.opaque.Castable.<:>
import com.avsystem.commons.serialization.GenCodec

private[opaque] trait BaseOpaque[From] extends Castable.Ops {
  trait Tag
  type Type

  def apply(value: From): Type


  implicit protected final val castable: From <:> Type = new Castable[From, Type]
  implicit final def codec(implicit fromCodec: GenCodec[From]): GenCodec[Type] = wrapF(fromCodec)
}
