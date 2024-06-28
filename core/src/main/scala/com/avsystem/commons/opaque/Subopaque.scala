package com.avsystem.commons
package opaque

trait Subopaque[From] extends BaseOpaque[From] {

  final type Type = From & Tag
  def apply(value: From): Type
}

object Subopaque {

  trait Default[From] extends Subopaque[From] {
    override final def apply(value: From): Type = wrap(value)
  }
}
