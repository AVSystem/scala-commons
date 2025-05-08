package com.avsystem.commons
package opaque

import com.avsystem.commons.opaque.Castable.<:>
import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec}

private[opaque] trait BaseOpaque[From] extends Castable.Ops {
  trait Tag
  type Type

  def apply(value: From): Type


  implicit protected final val castable: From <:> Type = new Castable[From, Type]
  implicit final def transparentCodec(implicit fromCodec: GenCodec[From]): GenCodec[Type] = wrapF(fromCodec)
  implicit final def transparentKeyCodec(implicit fromKeyCodec: GenKeyCodec[From]): GenKeyCodec[Type] = wrapF(fromKeyCodec)
}
