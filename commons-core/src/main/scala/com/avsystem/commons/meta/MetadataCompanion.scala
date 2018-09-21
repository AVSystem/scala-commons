package com.avsystem.commons
package meta

import com.avsystem.commons.macros.misc.MiscMacros
import com.avsystem.commons.misc.ImplicitNotFound
import com.avsystem.commons.rpc.Fallback

import scala.annotation.implicitNotFound

trait MetadataCompanion[M[_]] {
  final def apply[Real](implicit metadata: M[Real]): M[Real] = metadata

  implicit final def fromFallback[Real](implicit fallback: Fallback[M[Real]]): M[Real] = fallback.value

  final class Lazy[Real](metadata: => M[Real]) {
    lazy val value: M[Real] = metadata
  }
  object Lazy {
    def apply[Real](metadata: => M[Real]): Lazy[Real] = new Lazy(metadata)

    // macro effectively turns `metadata` param into by-name param (implicit params by themselves cannot be by-name)
    implicit def lazyMetadata[Real](implicit metadata: M[Real]): Lazy[Real] = macro MiscMacros.lazyMetadata

    @implicitNotFound("#{forNotLazy}")
    implicit def notFound[T](implicit forNotLazy: ImplicitNotFound[M[T]]): ImplicitNotFound[Lazy[T]] =
      ImplicitNotFound()
  }
}
