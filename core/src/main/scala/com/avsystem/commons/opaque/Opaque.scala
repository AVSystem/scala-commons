package com.avsystem.commons
package opaque

import com.avsystem.commons.opaque.Opaque.Hidden

trait Opaque[From] extends BaseOpaque[From] {

  final type Type = Hidden[From, Tag]
}

object Opaque {

  type Hidden[From, Tag]
  @inline implicit def classTag[From, Tag](implicit base: ClassTag[From]): ClassTag[Hidden[From, Tag]] = ClassTag(base.runtimeClass)

  trait Default[From] extends Opaque[From] {
    override final def apply(value: From): Type = wrap(value)
  }
}